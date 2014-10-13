
import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.TemplateHaskell
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import Conversion
import Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()

import qualified Type.Feldspar.GADT                  as TFG
import Compiler (scompileWith)

import qualified Expression.Feldspar.MiniWellScoped  as FMWS
import qualified Type.Feldspar.ADT                   as TFA
import Expression.Feldspar.Conversion ()

fft :: Data (Ary Complex -> Ary Complex)
fft = [|| \ vv -> $$forLoop 1 vv
          (\ _j -> \ v ->
                  ary (len v) (\ i -> ind v i))
      ||]

dummyAry :: Ary Complex
dummyAry = dummyAry

fftFMWS :: FMWS.Exp (TFA.Ary TFA.Cmx ': Prelude) (TFA.Ary TFA.Cmx)
fftFMWS = MP.frmRgt (cnv ([|| $$fft dummyAry  ||]
                         , TFG.Ary TFG.Cmx <:> etTFG
                         , 'dummyAry <+> esTH))

c :: MP.String
c = let f = MP.frmRgt (scompileWith [("v0" , TFA.Ary TFA.Cmx)]
                        (TFG.Ary TFG.Cmx)
                        ("v0" <+> esString) 1
                        fftFMWS)
        f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
    in f'

main :: MP.IO ()
main = MP.writeFile "MinimalTemplateHaskell.c" c

{-
-- The final AST
f v = Let (Whl
           (\ _x0 ->
            Let (Fst _x0)
            (\ _x1 -> AppV v6 [_x1 , ConI 1]))
           (\ _x2 ->
            Let (Fst _x2)
            (\ _x3 ->
             Let (AppV v8 [_x3 , ConI 1])
             (\ _x4 ->
              Let (Snd _x2)
              (\ _x5 -> Let (Len _x5)
               (\ _x6 -> (Tpl _x4
                          (Ary
                           _x6
                           (\ _x7 -> Ind _x5 _x7))))))))
           (Tpl (ConI 0) v))
        (\ _x8 -> Snd _x8)


---(extensional rule) let x = M in N --> N [x := M] if count(x,N) == 1 --->

f v = Let (Whl
           (\ _x0 ->
            AppV v6 [Fst _x0 , ConI 1])
           (\ _x2 ->
            Let (Snd _x2)
            (\ _x5 ->
             (Tpl
              (AppV v8 [Fst _x2 , ConI 1])
              (Ary (Len _x5) (\ _x7 -> Ind _x5 _x7))
             )))
           (Tpl
            (ConI 0)
            v))
      (\ _x8 -> Snd _x8)


-}