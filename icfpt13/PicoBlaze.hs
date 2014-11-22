module PicoBlaze where

type Instruction = W18
type Register = W4
type Byte = W8
data Flag = FlagZ | FlagC | FlagZsave | FlagCsave | FlagIE -- deriving Eq

instance Eq Flag where
  FlagZ == FlagZ = True
  FlagC == FlagC = True
  FlagZsave == FlagZsave = True
  FlagCsave == FlagCsave = True
  FlagIE == FlagIE = True
  _ == _ = False

--
-- The CPU monad.
--
--type CPU = ReactT2 Outputs Inputs (StateT CPUState I)
type CPU = ReactT Outputs Inputs I
data Outputs = Outputs W10 W8 Bit W8 Bit Bit
data Inputs = Inputs W18 W8 Bit Bit
data CPUState = CPUState RegFile FlagFile Mem Stack Outputs Inputs

type RegFile = Table W4 W8              -- 16 regs named s0,s1,...,sE,sF
type FlagFile = (Bit,Bit,Bit,Bit,Bit)   -- Z, C, Zsave, Csave, IE
type Mem = Table W6 W8                  -- 64-byte scratchpad RAM

-- Control flow stack. It's a ring buffer without over/underflow protection.
-- Whatever is at "pos" is in fact the current program counter. Xilinx
-- describes it as a 31-element stack and acts as if PC is a separate reg,
-- but I bet this is how it's implemented. (It's functionally equivalent
-- anyway.)
data Stack = Stack (Table W5 W10) W5

contents :: Stack -> Table W5 W10
contents (Stack t _) = t

update_contents :: Stack -> (Table W5 W10) -> Stack
update_contents (Stack _ p) t = Stack t p 
pos :: Stack -> W5
pos (Stack _ p) = p

update_pos :: Stack -> W5 -> Stack
update_pos (Stack t _) p = Stack t p

reg_file :: CPUState -> RegFile
reg_file (CPUState r _ _ _ _ _ ) = r

update_registers :: CPUState -> RegFile -> CPUState
update_registers (CPUState _ a1 a2 a3 a4 a5 ) r = CPUState r a1 a2 a3 a4 a5

flags    :: CPUState -> FlagFile
flags (CPUState _ f _ _ _ _ ) = f

update_flags :: CPUState -> FlagFile -> CPUState
update_flags (CPUState a1 _ a2 a3 a4 a5) f = (CPUState a1 f a2 a3 a4 a5)

memory   :: CPUState -> Mem
memory (CPUState _ _ m _ _ _ ) = m

update_memory :: CPUState -> Mem -> CPUState
update_memory (CPUState a1 a2 _ a3 a4 a5 ) m = CPUState a1 a2 m a3 a4 a5

stack    :: CPUState -> Stack
stack (CPUState _ _ _ s _ _ ) = s 

update_stack :: CPUState -> Stack -> CPUState
update_stack (CPUState a1 a2 a3 _ a4 a5) s = CPUState a1 a2 a3 s a4 a5

outputs  :: CPUState -> Outputs
outputs (CPUState _ _ _ _ o _ ) = o

update_outputs :: CPUState -> Outputs -> CPUState
update_outputs (CPUState a1 a2 a3 a4 _ a5) o = CPUState a1 a2 a3 a4 o a5 

inputs   :: CPUState -> Inputs
inputs (CPUState _ _ _ _ _ i) = i

update_inputs  :: CPUState -> Inputs -> CPUState
update_inputs (CPUState a1 a2 a3 a4 a5 _) i = CPUState a1 a2 a3 a4 a5 i 

instruction_in :: Inputs -> W18
instruction_in (Inputs i _ _ _) = i

in_port_in     :: Inputs -> W8
in_port_in (Inputs _ i _ _) = i

interrupt_in   :: Inputs -> Bit
interrupt_in (Inputs _ _ i _) = i

reset_in       :: Inputs -> Bit
reset_in (Inputs _ _ _ i) = i

address_out       :: Outputs -> W10
address_out (Outputs o _ _ _ _ _) = o

update_address_out :: Outputs -> W10 -> Outputs
update_address_out (Outputs _ a1 a2 a3 a4 a5) o = Outputs o a1 a2 a3 a4 a5

port_id_out       :: Outputs -> W8
port_id_out (Outputs _ o _ _ _ _) = o

update_port_id_out :: Outputs -> W8 -> Outputs
update_port_id_out (Outputs a1 _ a2 a3 a4 a5) o = Outputs a1 o a2 a3 a4 a5

write_strobe_out  :: Outputs -> Bit
write_strobe_out (Outputs _ _ o _ _ _) = o

update_write_strobe_out :: Outputs -> Bit -> Outputs
update_write_strobe_out (Outputs a1 a2 _ a3 a4 a5) o = Outputs a1 a2 o a3 a4 a5

out_port_out      :: Outputs -> W8
out_port_out (Outputs _ _ _ o _ _) = o

update_out_port_out :: Outputs -> W8 -> Outputs
update_out_port_out (Outputs a1 a2 a3 _ a4 a5) o = Outputs a1 a2 a3 o a4 a5

read_strobe_out   :: Outputs -> Bit
read_strobe_out (Outputs _ _ _ _ o _) = o

update_read_strobe_out :: Outputs -> Bit -> Outputs
update_read_strobe_out (Outputs a1 a2 a3 a4 _ a5) o = Outputs a1 a2 a3 a4 o a5

interrupt_ack_out :: Outputs -> Bit
interrupt_ack_out (Outputs _ _ _ _ _ o) = o

update_interrupt_ack_out :: Outputs -> Bit -> Outputs
update_interrupt_ack_out (Outputs a1 a2 a3 a4 a5 _) o = Outputs a1 a2 a3 a4 a5 o

getFlag :: CPUState -> Flag -> CPU (Bit,CPUState)
getFlag cpu f = case flags cpu of
                 (fz,fc,fzs,fcs,fie) ->
                  case f of
                    FlagZ     -> return (fz,cpu)
                    FlagC     -> return (fc,cpu)
                    FlagZsave -> return (fzs,cpu)
                    FlagCsave -> return (fcs,cpu)
                    FlagIE    -> return (fie,cpu)

putFlag :: CPUState -> Flag -> Bit -> CPU ((),CPUState)
putFlag cpu f b = case flags cpu of
                   (fz,fc,fzs,fcs,fie) ->
                     case f of
                       FlagZ     -> return $ ((),update_flags cpu (b,fc,fzs,fcs,fie))
                       FlagC     -> return $ ((),update_flags cpu (fz,b,fzs,fcs,fie))
                       FlagZsave -> return $ ((),update_flags cpu (fz,fc,b,fcs,fie))
                       FlagCsave -> return $ ((),update_flags cpu (fz,fc,fzs,b,fie))
                       FlagIE    -> return $ ((),update_flags cpu (fz,fc,fzs,fcs,b))

getFlagFile :: CPUState -> CPU (FlagFile,CPUState)
getFlagFile cpu = return cpu >>= \cpu' -> return (flags cpu',cpu')

putFlagFile :: CPUState -> FlagFile -> CPU ((),CPUState)
putFlagFile cpu ff = return ((),update_flags cpu ff) --lift (u (\ s -> s { flags = ff }))

getReg :: CPUState -> Register -> CPU (Byte,CPUState)
getReg cpu r = return (reg_file cpu) >>= \y -> return ((y!r),cpu)

putReg :: CPUState -> Register -> Byte -> CPU ((),CPUState)
putReg cpu r b = return (reg_file cpu) >>= \tbl -> (return ((),update_registers cpu (tbl <| (r,b))))

getRegFile :: CPUState -> CPU (RegFile,CPUState)
getRegFile cpu = return (reg_file cpu,cpu) --lift (g >>= return . reg_file)

putRegFile :: CPUState -> RegFile -> CPU ((),CPUState)
putRegFile cpu rf = return ((),update_registers cpu rf) --lift (u (\ s -> s { reg_file = rf }))

getStack :: CPUState -> CPU (Stack,CPUState)
getStack cpu = return (stack cpu,cpu) -- >>= return -- to keep it to two "ticks"?

putStack :: CPUState -> Stack -> CPU ((),CPUState)
putStack cpu st = return $ ((),update_stack cpu st) 

push :: CPUState -> W10 -> CPU ((),CPUState)
push cpu a = getStack cpu  >>= \(st,_) ->
                let p' = pos st + 1
                    c' = contents st <| (p',a) in
                putStack cpu (Stack c' p')

pop :: CPUState -> CPU ((),CPUState)
pop cpu = getStack cpu >>= \(st,_) ->
             putStack cpu $ update_pos st (pos st - 1)

getPC :: CPUState -> CPU (W10,CPUState)
getPC cpu = getStack cpu >>= \(st,_) ->
               let p = pos st
                   c = contents st in
               return ((c ! p),cpu)

putPC :: CPUState -> W10 -> CPU ((),CPUState)
putPC cpu pc' = getStack cpu >>= \(st,_) ->
                   let p   = pos st
                       c   = contents st
                       st' = update_contents st (c <| (p,pc')) in
                   putStack cpu st'

incrPC :: CPUState -> CPU ((),CPUState)
incrPC cpu = getPC cpu >>= \(pc,_) ->
                putPC cpu (pc+1)


getRAM :: CPUState -> CPU (Mem,CPUState)
getRAM cpu = return $ (memory cpu, cpu)

putRAM :: CPUState -> Mem -> CPU ((),CPUState)
putRAM cpu m = return ((),update_memory cpu m) 

putToRAM :: CPUState -> W6 -> Byte -> CPU ((),CPUState)
putToRAM cpu a b = getRAM cpu >>= \(ram,_) ->
                     putRAM cpu (ram <| (a,b))

getFromRAM :: CPUState -> W6 -> CPU (Byte,CPUState)
getFromRAM cpu a = getRAM cpu >>= \(r,_) -> return ((r ! a),cpu)



writePortID :: CPUState -> Byte -> CPU ((),CPUState)
writePortID cpu b = return ((),update_outputs cpu $ update_port_id_out (outputs cpu) b) --lift (u (\ s -> s { outputs = (outputs s) { port_id_out = b } }))

readInPort :: CPUState -> CPU (Byte,CPUState)
readInPort cpu = return cpu >>= \s -> 
                    return (in_port_in (inputs s),cpu)

writeOutPort :: CPUState -> Byte -> CPU ((),CPUState)
writeOutPort cpu b = return ((),update_outputs cpu $ update_out_port_out (outputs cpu) b) --lift (u (\ s -> s { outputs = (outputs s) { out_port_out = b } }))

tick :: CPUState -> CPU ((),CPUState)
tick cpu = getPC cpu >>= \(pc,_) ->
              --lift (u (\ s -> s { outputs = (outputs s) { address_out = pc } }))
              let cpu' = update_outputs cpu $ update_address_out (outputs cpu) pc in
              --s  <- lift g
              signal (outputs cpu') >>= \i -> 
              let cpu'' = update_inputs cpu' i in
              --lift (u (\ s -> s { inputs = i }))
              return ((),cpu'')

-- A few utility functions.
toBit :: Bool -> Bit
toBit True  = One
toBit False = Zero

bitToByte :: Bit -> Byte
bitToByte 0 = 0
bitToByte 1 = 1

--
-- Instruction decoding. (See PicoBlaze User Guide, pp. 115-117 for
-- instruction encoding.) Would be nice if we had some kind of bit-slicing
-- patterns here!
--
decode :: CPUState -> Instruction -> CPU ((),CPUState)
decode cpu (W18  0  1  1  0  0  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  addImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  1  0  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  addReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  1  1  0  1  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  addImmC cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  1  0  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  addRegC cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  0  1  0  1  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  andImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  0  1  0  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  andReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  1  1  0  0  0  0  0  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  call cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  0  0  1  1  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  callC cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  0  0  1  1  1 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  callNC cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  0  0  1  0  1 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  callNZ cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  0  0  1  0  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  callZ cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  0  1  0  1  0  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  compareImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  0  1  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  compareReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  1  1  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  b) =
  setInterruptEnable cpu b  -- both EINT and DINT are handled here
decode cpu (W18  0  0  0  1  1  0 x0 x1 x2 x3  0  0 s0 s1 s2 s3 s4 s5) =
  fetchImm cpu (W4 x0 x1 x2 x3) (W6 s0 s1 s2 s3 s4 s5)
decode cpu (W18  0  0  0  1  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  fetchReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  0  0  1  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  inputReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  0  0  1  0  0 x0 x1 x2 x3 p0 p1 p2 p3 p4 p5 p6 p7) =
  inputImm cpu (W4 x0 x1 x2 x3) (W8 p0 p1 p2 p3 p4 p5 p6 p7)
decode cpu (W18  1  1  0  1  0  0  0  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  jump cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  1  0  1  1  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  jumpC cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  1  0  1  1  1 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  jumpNC cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  1  0  1  0  1 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  jumpNZ cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  1  1  0  1  0  1  0  0 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) =
  jumpZ cpu (W10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
decode cpu (W18  0  0  0  0  0  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  loadImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  0  0  0  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  loadReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  0  1  1  0  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  orImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  0  1  1  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  orReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  1  0  1  1  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  outputReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  1  0  1  1  0  0 x0 x1 x2 x3 p0 p1 p2 p3 p4 p5 p6 p7) =
  outputImm cpu (W4 x0 x1 x2 x3) (W8 p0 p1 p2 p3 p4 p5 p6 p7)
decode cpu (W18  1  0  1  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0) =
  ret cpu 
decode cpu (W18  1  0  1  0  1  1  1  0  0  0  0  0  0  0  0  0  0  0) =
  retC cpu 
decode cpu (W18  1  0  1  0  1  1  1  1  0  0  0  0  0  0  0  0  0  0) =
  retNC cpu 
decode cpu (W18  1  0  1  0  1  1  0  1  0  0  0  0  0  0  0  0  0  0) =
  retNZ cpu 
decode cpu (W18  1  0  1  0  1  1  0  0  0  0  0  0  0  0  0  0  0  0) =
  retZ cpu 
decode cpu (W18  1  1  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  b) =
  reti cpu b -- both RETI DISABLE and RETI ENABLE are handled here
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  0  0  1  0) =
  rl cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  1  1  0  0) =
  rr cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  0  1  1  0) =
  sl0 cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  0  1  1  1) =
  sl1 cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  0  0  0  0) =
  sla cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  0  1  0  0) =
  slx cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  1  1  1  0) =
  sr0 cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  1  1  1  1) =
  sr1 cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  1  0  0  0) =
  sra cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  0  0  0  0 x0 x1 x2 x3  0  0  0  0  1  0  1  0) =
  srx cpu (W4 x0 x1 x2 x3)
decode cpu (W18  1  0  1  1  1  0 x0 x1 x2 x3  0  0 s0 s1 s2 s3 s4 s5) =
  storeImm cpu (W4 x0 x1 x2 x3) (W6 s0 s1 s2 s3 s4 s5)
decode cpu (W18  1  0  1  1  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  storeReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  1  1  1  0  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  subImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  1  1  0  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  subReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  1  1  1  1  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  subImmC cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  1  1  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  subRegC cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  1  0  0  1  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  testImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  1  0  0  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  testReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu (W18  0  0  1  1  1  0 x0 x1 x2 x3 k0 k1 k2 k3 k4 k5 k6 k7) =
  xorImm cpu (W4 x0 x1 x2 x3) (W8 k0 k1 k2 k3 k4 k5 k6 k7)
decode cpu (W18  0  0  1  1  1  1 x0 x1 x2 x3 y0 y1 y2 y3  0  0  0  0) =
  xorReg cpu (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3)
decode cpu _                                                           =
  invalid cpu 

--
-- Binop-type instructions (register-register and register-immediate).
--



type Binop = Byte -> Byte -> Bit -> (Bit,Byte) -- (sX, sY, carry-in)

binopImm :: CPUState -> Binop -> Register -> W8 -> CPU ((),CPUState)
binopImm cpu oper sX kk = getReg cpu sX >>= \(v,_) -> 
                             getFlag cpu FlagC >>= \(c,_) ->
                             case (v `oper` kk) c of
                               (c',v') ->
                                 putFlag cpu FlagZ (toBit (v' == 0)) >>= \(_,cpu')  -> 
                                 putFlag cpu' FlagC c' >>= \(_,cpu'') -> 
                                 putReg cpu'' sX v' >>= \(_,cpu''') -> 
                                 incrPC cpu''' >>= \(_,cpu'''') -> 
                                 tick cpu'''' >>= \(_,cpu''''') -> 
                                 tick cpu'''''

binopReg :: CPUState -> Binop -> Register -> Register -> CPU ((),CPUState)
binopReg cpu oper sX sY = 
                             getReg cpu sX >>= \(vX,_) -> 
                             getReg cpu sY >>= \(vY,_) -> 
                             getFlag cpu FlagC >>= \(c,_) -> 
                             case (vX `oper` vY) c of
                               (c',vX') ->
                                 putFlag cpu  FlagZ (toBit (vX' == 0)) >>= \(_,cpu') -> 
                                 putFlag cpu' FlagC c' >>= \(_,cpu'') -> 
                                 putReg cpu'' sX vX' >>= \(_,cpu''') -> 
                                 incrPC cpu''' >>= \(_,cpu'''') -> 
                                 tick cpu'''' >>= \(_,cpu''''') -> 
                                 tick cpu'''''

-- Arithmetic instructions (add, sub).
opAdd,opAddC,opSub,opSubC :: Binop
opAdd  = prim "opAdd"
opAddC = prim "opAddC"
opSub  = prim "opSub"
opSubC = prim "opSubC"

addImm,addImmC,subImm,subImmC :: CPUState -> Register -> Byte -> CPU ((),CPUState)
addImm  cpu = binopImm cpu opAdd
addImmC cpu = binopImm cpu opAddC
subImm  cpu = binopImm cpu opSub
subImmC cpu = binopImm cpu opSubC

addReg,addRegC,subReg,subRegC :: CPUState -> Register -> Register -> CPU ((),CPUState)
addReg  cpu = binopReg cpu opAdd
addRegC cpu = binopReg cpu opAddC
subReg  cpu = binopReg cpu opSub
subRegC cpu = binopReg cpu opSubC

-- Logical instructions (and, or, xor). Note that these all set the carry
-- flag to zero and ignore carry-in.
opAnd,opOr,opXor :: Binop
opAnd = prim "opAnd"
opOr  = prim "opOr"
opXor = prim "opXor"
{-opAnd x y _ = (0,x .&. y)
opOr x y _  = (0,x .|. y)
opXor x y _ = (0,x `xor` y)-}

andImm,orImm,xorImm :: CPUState -> Register -> Byte -> CPU ((),CPUState)
andImm cpu = binopImm cpu opAnd
orImm  cpu = binopImm cpu opOr
xorImm cpu = binopImm cpu opXor

andReg,orReg,xorReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
andReg cpu = binopReg cpu opAnd
orReg  cpu = binopReg cpu opOr
xorReg cpu = binopReg cpu opXor

--
-- Bit-shift instructions (rotate left/right, shift left/right; the shift
-- instructions can be zero- or one-fill, spill into carry[?], or
-- sign-extend [arithmetic shift]).
--
type Unop = Byte -> Bit -> (Bit,Byte) -- the Bit argument is carry-in

doUnop :: CPUState -> Unop -> Register -> CPU ((),CPUState)
doUnop cpu oper sX = getReg cpu sX >>= \(v,_)           -> 
                        getFlag cpu FlagC >>= \(c,_)           -> 
                        case oper v c of
                          (c',v') ->
                            putFlag cpu FlagZ (toBit (v' == 0)) >>= \(_,cpu')  -> 
                            putFlag cpu' FlagC c' >>= \(_,cpu'') -> 
                            putReg cpu'' sX v' >>= \(_,cpu''') -> 
                            incrPC cpu''' >>= \(_,cpu'''') -> 
                            tick cpu'''' >>= \(_,cpu''''') -> 
                            tick cpu'''''

opRL,opRR,opSL0,opSL1,opSLA,opSLX,opSR0,opSR1,opSRA,opSRX :: Unop
opRL (W8 a b c d e f g h) _    = (a,W8 b c d e f g h a)
opRR (W8 a b c d e f g h) _    = (h,W8 h a b c d e f g)
opSL0 (W8 a b c d e f g h) _   = (a,W8 b c d e f g h 0)
opSL1 (W8 a b c d e f g h) _   = (a,W8 b c d e f g h 1)
opSLA (W8 a b c d e f g h) cin = (a,W8 b c d e f g h cin)
opSLX (W8 a b c d e f g h) _   = (a,W8 b c d e f g h h)
opSR0 (W8 a b c d e f g h) _   = (h,W8 0 a b c d e f g)
opSR1 (W8 a b c d e f g h) _   = (h,W8 1 a b c d e f g)
opSRA (W8 a b c d e f g h) cin = (h,W8 cin a b c d e f g)
opSRX (W8 a b c d e f g h) _   = (h,W8 a a b c d e f g)

rl,rr,sl0,sl1,sla,slx,sr0,sr1,sra,srx :: CPUState -> Register -> CPU ((),CPUState)
rl  cpu = doUnop cpu opRL
rr  cpu = doUnop cpu opRR
sl0 cpu = doUnop cpu opSL0
sl1 cpu = doUnop cpu opSL1
sla cpu = doUnop cpu opSLA
slx cpu = doUnop cpu opSLX
sr0 cpu = doUnop cpu opSR0
sr1 cpu = doUnop cpu opSR1
sra cpu = doUnop cpu opSRA
srx cpu = doUnop cpu opSRX

--
-- Test/comparison instructions. (These differ from binop instructions in
-- that theynot alter the contents of the registers.)
--

byteLT :: Byte -> Byte -> Bool
byteLT = prim "byteLT"

doCompare :: CPUState -> Byte -> Byte -> CPU ((),CPUState)
doCompare cpu x y = putFlag cpu FlagZ (toBit (x == y)) >>= \(_,cpu')   -> 
                       putFlag cpu' FlagC (toBit (x `byteLT` y)) >>= \(_,cpu'')  -> 
                       incrPC cpu'' >>= \(_,cpu''') -> 
                       tick cpu''' >>= \(_,cpu'''') -> 
                       tick cpu''''

compareImm :: CPUState -> Register -> Byte -> CPU ((),CPUState)
compareImm cpu sX kk =  getReg cpu sX >>= \(v,_) -> 
                          doCompare cpu v kk

compareReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
compareReg cpu sX sY = getReg cpu sX >>= \(vX,_) -> 
                          getReg cpu sY >>= \(vY,_) -> 
                          doCompare cpu vX vY

oddParity :: Byte -> Bit
oddParity (W8 b0 b1 b2 b3 b4 b5 b6 b7) = b0 `xor` b1 `xor` b2 `xor` b3 `xor` b4 `xor` b5 `xor` b6 `xor` b7

byteAnd :: Byte -> Byte -> Byte
byteAnd = prim "byteAnd"

doTest :: CPUState -> Byte -> Byte -> CPU ((),CPUState)
doTest cpu x y = putFlag cpu FlagZ (toBit (x `byteAnd` y == 0)) >>= \(_,cpu') -> 
                    putFlag cpu' FlagC (oddParity (x `byteAnd` y)) >>= \(_,cpu'') -> 
                    incrPC cpu'' >>= \(_,cpu''') -> 
                    tick cpu''' >>= \(_,cpu'''') -> 
                    tick cpu''''

testImm :: CPUState -> Register -> Byte -> CPU ((),CPUState)
testImm cpu sX kk = getReg cpu sX >>= \(v,_) -> 
                       doTest cpu v kk

testReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
testReg cpu sX sY = getReg cpu sX >>= \(vX,_) -> 
                       getReg cpu sY >>= \(vY,_) -> 
                       doTest cpu vX vY

--
-- Load (including reg-reg moves), store, and fetch instructions.
--
loadImm :: CPUState -> Register -> Byte -> CPU ((),CPUState)
loadImm cpu sX vX = putReg cpu sX vX >>= \(_,cpu') -> 
                       incrPC cpu' >>= \(_,cpu'') -> 
                       tick cpu'' >>= \(_,cpu''') -> 
                       tick cpu'''

loadReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
loadReg cpu sX sY = getReg cpu sY >>= \(vY,_) -> 
                       putReg cpu sX vY >>= \(_,cpu') -> 
                       incrPC cpu' >>= \(_,cpu'') -> 
                       tick cpu'' >>= \(_,cpu''') -> 
                       tick cpu'''

trim8to6 :: W8 -> W6
trim8to6 (W8 a b c d e f g h) = W6 c d e f g h


doFetch :: CPUState -> Register -> W6 -> CPU ((),CPUState)
doFetch cpu sX a = getFromRAM cpu a >>= \(vX',_) -> 
                      putReg cpu sX vX' >>= \(_,cpu') -> 
                      incrPC cpu' >>= \(_,cpu'') -> 
                      tick cpu'' >>= \(_,cpu''') -> 
                      tick cpu'''

fetchImm :: CPUState -> Register -> W6 -> CPU ((),CPUState)
fetchImm    = doFetch

fetchReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
fetchReg cpu sX sY = getReg cpu sY >>= \(a,_) -> 
                        doFetch cpu sX (trim8to6 a)

storeImm :: CPUState -> Register -> W6 -> CPU ((),CPUState)
storeImm cpu sX ss = getReg cpu sX >>= \(v,_) -> 
                        putToRAM cpu ss v >>= \(_,cpu') -> 
                        incrPC cpu' >>= \(_,cpu'') -> 
                        tick cpu'' >>= \(_,cpu''') -> 
                        tick cpu'''

storeReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
storeReg cpu sX sY = getReg cpu sX >>= \(v,_) -> 
                        getReg cpu sY >>= \(ss,_) -> 
                        putToRAM cpu (trim8to6 ss) v >>= \(_,cpu') -> 
                        incrPC cpu' >>= \(_,cpu'') -> 
                        tick cpu'' >>= \(_,cpu''') -> 
                        tick cpu'''

--
-- Control-flow instructions.
--
whenFlag :: CPUState -> Flag -> CPU a -> CPU a -> CPU a
whenFlag cpu f phi gamma = getFlag cpu f >>= \(v,_) -> 
                              case v of
                                1 -> phi
                                0 -> gamma

jump :: CPUState -> W10 -> CPU ((),CPUState)
jump cpu a   = putPC cpu a >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

jumpC :: CPUState -> W10 -> CPU ((),CPUState)
jumpC cpu a  = whenFlag cpu FlagC (putPC cpu a) (incrPC cpu) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

jumpNC :: CPUState -> W10 -> CPU ((),CPUState)
jumpNC cpu a = whenFlag cpu FlagC (incrPC cpu) (putPC cpu a) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

jumpNZ :: CPUState -> W10 -> CPU ((),CPUState)
jumpNZ cpu a = whenFlag cpu FlagZ (incrPC cpu) (putPC cpu a) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

jumpZ :: CPUState -> W10 -> CPU ((),CPUState)
jumpZ cpu a  = whenFlag cpu FlagZ (putPC cpu a) (incrPC cpu) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''


call :: CPUState -> W10 -> CPU ((),CPUState)
call cpu a   = push cpu a >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

callC :: CPUState -> W10 -> CPU ((),CPUState)
callC cpu a  = whenFlag cpu FlagC (push cpu a) (incrPC cpu) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

callNC :: CPUState -> W10 -> CPU ((),CPUState)
callNC cpu a = whenFlag cpu FlagC (incrPC cpu) (push cpu a) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

callNZ :: CPUState -> W10 -> CPU ((),CPUState)
callNZ cpu a = whenFlag cpu FlagZ (incrPC cpu) (push cpu a) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

callZ :: CPUState -> W10 -> CPU ((),CPUState)
callZ  cpu a = whenFlag cpu FlagZ (push cpu a) (incrPC cpu) >>= \(_,cpu') -> 
                  tick cpu' >>= \(_,cpu'') -> 
                  tick cpu''

ret :: CPUState -> CPU ((),CPUState)
ret cpu   = pop cpu >>= \(_,cpu') -> 
               incrPC cpu' >>= \(_,cpu'') -> 
               tick cpu'' >>= \(_,cpu''') -> 
               tick cpu'''

retC :: CPUState -> CPU ((),CPUState)
retC cpu  = whenFlag cpu FlagC (pop cpu) (return ((),cpu)) >>= \(_,cpu') -> 
               incrPC cpu' >>= \(_,cpu'') -> 
               tick cpu'' >>= \(_,cpu''') -> 
               tick cpu'''

retNC :: CPUState -> CPU ((),CPUState)
retNC cpu  = whenFlag cpu FlagC (return ((),cpu)) (pop cpu) >>= \(_,cpu') -> 
                 incrPC cpu' >>= \(_,cpu'') -> 
                 tick cpu'' >>= \(_,cpu''') -> 
                 tick cpu'''
                 
retNZ :: CPUState -> CPU ((),CPUState)
retNZ  cpu = whenFlag cpu FlagZ (return ((),cpu)) (pop cpu) >>= \(_,cpu') -> 
                incrPC cpu' >>= \(_,cpu'') -> 
                tick cpu'' >>= \(_,cpu''') -> 
                tick cpu'''

retZ :: CPUState -> CPU ((),CPUState)
retZ cpu  = whenFlag cpu FlagZ (pop cpu) (return ((),cpu)) >>= \(_,cpu') -> 
                incrPC cpu' >>= \(_,cpu'') -> 
                tick cpu'' >>= \(_,cpu''') -> 
                tick cpu'''

reti :: CPUState -> Bit -> CPU ((),CPUState)
reti cpu b =
                getFlag cpu FlagZsave >>= \(f,_) -> 
                putFlag cpu FlagZ f  >>= \(_,cpu') -> 
                getFlag cpu' FlagCsave >>= \(f',_) -> 
                putFlag cpu' FlagC f' >>= \(_,cpu'') -> 
                putFlag cpu'' FlagIE b >>= \(_,cpu''') -> 
                pop cpu''' >>= \(_,cpu'''') -> 
                tick cpu'''' >>= \(_,cpu''''') -> 
                tick cpu'''''

--
-- I/O-related instructions.
--
setInterruptEnable :: CPUState -> Bit -> CPU ((),CPUState)
setInterruptEnable cpu b = putFlag cpu FlagIE b

inputReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
inputReg cpu sX sY = getReg cpu sY >>= \(vY,_) -> 
                        writePortID cpu vY >>= \(_,cpu') -> 
                        incrPC cpu' >>= \(_,cpu'') -> 
                        tick cpu'' >>= \(_,cpu''') -> 
                        readInPort cpu''' >>= (\(p,_) -> putReg cpu''' sX p) >>= \(_,cpu'''') -> 
                        tick cpu''''

inputImm :: CPUState -> Register -> W8 -> CPU ((),CPUState)
inputImm cpu sX pp = writePortID cpu pp >>= \(_,cpu') -> 
                        incrPC cpu' >>= \(_,cpu'') -> 
                        tick cpu'' >>= \(_,cpu''') -> 
                        readInPort cpu''' >>= \(p,_) -> putReg cpu''' sX p >>= \(_,cpu'''') -> 
                        tick cpu''''

outputReg :: CPUState -> Register -> Register -> CPU ((),CPUState)
outputReg cpu sX sY = getReg cpu sY >>= writePortID cpu . fst >>= \(_,cpu') -> 
                         getReg cpu' sX >>= writeOutPort cpu' . fst >>= \(_,cpu'') -> 
                         incrPC cpu'' >>= \(_,cpu''') -> 
                         tick cpu''' >>= \(_,cpu'''') -> 
                         tick cpu''''

outputImm :: CPUState -> Register -> W8 -> CPU ((),CPUState)
outputImm cpu sX pp = writePortID cpu pp  >>= \(_,cpu') -> 
                         getReg cpu' sX >>= writeOutPort cpu' . fst >>= \(_,cpu'') -> 
                         incrPC cpu'' >>= \(_,cpu''') -> 
                         tick cpu''' >>= \(_,cpu'''') -> 
                         tick cpu''''

--
-- Invalid instructions.
--
invalid :: CPUState -> CPU ((),CPUState)
invalid cpu = incrPC cpu >>= \(_,cpu) -> tick cpu >>= \(_,cpu) -> tick cpu

--
-- Main "fetch-decode-execute" loop.
--
machine_start :: CPUState -> CPU ((),CPUState)
machine_start cpu = return cpu  >>= \s -> 
                      (let i     = inputs s
                           instr = instruction_in i in
                       getFlag s FlagIE >>= \(ie,_) -> 
                       if (reset_in i) == 1
                         then reset_event cpu
                         else if (ie == 1) && (interrupt_in i == 1)
                                then interrupt_event cpu
                                else decode cpu instr) >>= \(_,cpu') -> machine_start cpu'

interrupt_event :: CPUState -> CPU ((),CPUState)
interrupt_event cpu = ((\(f,cpu) -> putFlag cpu FlagZsave f) =<< getFlag cpu FlagZ) >>= \(_,cpu') -> 
                         ((\(f,cpu) -> putFlag cpu FlagCsave f) =<< getFlag cpu' FlagC) >>= \(_,cpu'') -> 
                         putFlag cpu'' FlagIE 0 >>= \(_,cpu''') -> 
                         push cpu''' 0x3ff >>= \(_,cpu'''') -> 
                         tick cpu'''' >>= \(_,cpu''''') -> 
                         tick cpu'''''

reset_event :: CPUState -> CPU ((),CPUState)
reset_event cpu = putPC cpu 0 >>= \(_,cpu') -> 
                     putFlag cpu' FlagZ 0 >>= \(_,cpu'') -> 
                     putFlag cpu'' FlagC 0 >>= \(_,cpu''') -> 
                     putFlag cpu''' FlagIE 0 >>= \(_,cpu'''') -> 
                     tick cpu'''' >>= \(_,cpu''''') -> 
                     tick cpu'''''

{-                         
roll :: ReactT2 req rsp (StateT sto I) a -> sto -> ReactT req rsp I (a,sto)
roll (ReactT2 m) s = let (dp,s') = deId $ deST m s
                     in  case dp of
                           Left v      -> D (v,s')
                           Right (q,k) -> P q (\ rsp -> I $ roll (k rsp) s')
--roll (D v) s   = D (v,s)
--roll (P q r) s = P q (\ rsp -> deST (r rsp) s >>= return . uncurry roll)

-}

main :: ReactT Outputs Inputs I ((),CPUState)
main = machine_start initialCPUState
  where initialCPUState = CPUState (newTable 0) (0,0,0,0,0) (newTable 0) (Stack (newTable 0) 0) (Outputs 0 0 0 0 0 0) (Inputs 0 0 0 0)
