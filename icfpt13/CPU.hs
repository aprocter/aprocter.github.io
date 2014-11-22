module CPU where
--
-- The vanilla CPU
--

type ReT = ReactT
type StT = StateT

-- Architectural Types.
type InpSig = (--Bit,       -- external reset signal
               W16)       -- input word from address bus
type OutSig = (Bit,       -- LED off/on
               Addr)      -- output address to bus
type Addr   = W8
type RegF   = (W8,W8,W8) -- (r1,r2,pc)

type Re = ReT OutSig InpSig I
type CPUState = OutSig

-- Decoded instructions.
data Instr = Branch0 W8   -- branch to absolute target if r0=0
           | LoadR1 W8    -- load r1 from address
           | LoadR2 W8    -- load r2 from address
           | Add          -- r1 := r1+r2
           | SetLED       -- my led := lsb[r1]
           | Invalid      -- invalid instruction

split16 :: W16 -> (W8,W8)
split16 (W16(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,ba,bb,bc,bd,be,bf)) = (byt1,byt2)
   where byt1 = W8 (b0,b1,b2,b3,b4,b5,b6,b7)
         byt2 = W8 (b8,b9,ba,bb,bc,bd,be,bf)

decode :: W16 -> Instr
decode w = case split16 w of
                (0x80,byt) -> Branch0 byt
                (0x81,byt) -> LoadR1 byt
                (0x82,byt) -> LoadR2 byt
                (0x83,_)   -> Add
                (0x84,_)   -> SetLED
                (_,_)      -> Invalid

putPC pc (r1,r2,_) = (r1,r2,pc)
getPC (_,_,pc) = pc

putR1 r1 (_,r2,pc) = (r1,r2,pc)
getR1 (r1,_,_) = r1

putR2 r2 (r1,_,pc) = (r1,r2,pc)
getR2 (_,r2,_) = r2

putLED led (_,addr) = (led,addr)

putAddr addr (led,_) = (led,addr)

w8_zero = W8 (Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero)
w16_zero = W16 (Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero)

sc0 = (Zero,w8_zero)
su0 = (w8_zero,w8_zero,w8_zero)

machine_start :: Re ()
machine_start = stepRe sc0 (\ _ -> return (sc0,su0)) >>= machine_reset

machine_reset :: (CPUState,RegF) -> Re ()
machine_reset (sc,su) = let sc' = putLED Zero $ putAddr w8_zero sc
                            su' = putPC w8_zero su
                        in stepRe sc' (\ word -> return (sc',su')) >>= machine_fetch

machine_fetch :: (CPUState,RegF) -> Re ()
machine_fetch (sc,su) = let pc  = getPC su
                            sc' = putAddr pc sc
                            su' = putPC (pc+1) su
                        in stepRe sc' (\ word -> return (sc',su',decode word)) >>= machine_exec
                           
machine_exec :: (CPUState,RegF,Instr) -> Re ()
machine_exec (sc,su,Branch0 pc') = let r1  = getR1 su
                                       su' = case r1 of
                                               W8 (Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero) -> putPC pc' su
                                               _                                            -> su
                                   in stepRe sc (\ _ -> return (sc,su')) >>= machine_fetch

machine_exec (sc,su,i@(LoadR1 a)) = let sc' = putAddr a sc
                                    in stepRe sc' (\ _ -> return (sc',su,i)) >>=
                                       machine_exec_2

machine_exec (sc,su,i@(LoadR2 a)) = let sc' = putAddr a sc
                                    in stepRe sc' (\ _ -> return (sc',su,i)) >>=
                                       machine_exec_2

machine_exec (sc,su,Add) = let r1 = getR1 su
                               r2 = getR2 su
                               su' = putR1 (r1+r2) su
                           in stepRe sc (\ _ -> return (sc,su')) >>=
                              machine_fetch

machine_exec (sc,su,SetLED) = let r1  = getR1 su
                                  sc' = putLED (lsb r1) sc
                              in stepRe sc' (\ _ -> return (sc',su)) >>=
                                 machine_fetch

machine_exec (sc,su,Invalid) = stepRe sc (\ _ -> return (sc,su)) >>= machine_reset

trim (W16 (_,_,_,_,_,_,_,_,a,b,c,d,e,f,g,h)) = W8 (a,b,c,d,e,f,g,h)

lsb :: W8 -> Bit
lsb (W8 (_,_,_,_,_,_,_,b)) = b

machine_exec_2 :: (CPUState,RegF,Instr) -> Re ()
machine_exec_2 (sc,su,LoadR1 _) = stepRe sc (\ word -> let su' = putR1 (trim word) su
                                                             in return (sc,su')) >>=
                                      machine_fetch

machine_exec_2 (sc,su,LoadR2 _) = stepRe sc (\ word -> let su' = putR2 (trim word) su
                                                             in return (sc,su')) >>=
                                      machine_fetch
