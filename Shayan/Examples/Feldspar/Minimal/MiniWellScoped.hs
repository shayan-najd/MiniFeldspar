{-# LANGUAGE RebindableSyntax #-}

import qualified MyPrelude as MP

import Examples.Feldspar.Prelude.MiniWellScoped
import Examples.Feldspar.Prelude.Environment
import Examples.Feldspar.FFT.Common

import qualified Type.Feldspar.GADT            as TFG
import Compiler(scompile)

fft :: Vec Complex -> Vec Complex
fft = \ vv -> forLoopVec 1 vv
      (\ _j -> \ v ->
              vec (lenV v) (\ i -> indV v i))

fftAry :: Data (Ary Complex) -> Data (Ary Complex)
fftAry = vec2ary MP.. fft MP.. ary2vec

c :: MP.String
c = let f = MP.frmRgt
            (scompile
             (TFG.Ary TFG.Cmx)
             esString
             fftAry)
        f' = "#include\"ppm.h\"\n" MP.++ f MP.++ loaderC
    in f'

main :: MP.IO ()
main = MP.writeFile "MinimalMiniWellScoped.c" c

{-
-- The final AST
f v = let _x8 = Whl
                 (\ _x0 ->
                  AppV v6 [Fst _x9 , ConI 1])
                 (\ _x2 ->
                  let _x5 = Snd _x2
                  in Tpl
                     (AppV v8 [Fst _x2 , ConI 1])
                     (Ary
                      (Len _x5)
                      (\ _x3 -> Ind (Snd _x2) _x3)))
                 (Tpl
                  (ConI 0)
                  (Ary (Len v) (\ _x4 -> Ind v _x4)))
       in  Ary
              (Len (Snd _x8))
              (\ _x5 -> Ind (Snd _x8) _x5)
-}