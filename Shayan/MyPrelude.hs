module MyPrelude 
       (Int,MyPrelude.Integer,Num(..),div,toInteger,pi,(/),floor,log,
        Word32,Rational, 
        Float,toRational,fromRational,fromIntegral,
        Bool(..),(&&),(||),not,
        Complex(..),realPart,imagPart,cis,magnitude,
        (.&.),(.|.),xor,shiftR,shiftL,complement,popCountDefault,testBit,
        IO,print,readFile,writeFile,putStrLn,
        fst,snd,
        String,lines,unlines,
        read,
        (.),flip,curry,uncurry,id,const,
        (++),zip,head,dropWhile,iterate,length,zipWith,filter,(!!),delete,init,
        Maybe(..),fromJust,maybe,
        Enum(..),
        Ord(..),
        Eq(..),
        Show(..),
        otherwise, impossible , impossibleM,
        lookup,
        State,getState,put,modify,runState,execState,evalState,StateT
             ,lift,runStateT,
        module ErrorMonad,
        module Existential,
        module Data.Monoid,
        module Data.Foldable, 
        module Data.Traversable,
        module Data.Functor,
        module Control.Applicative,
        module Control.Monad,
        module Data.Array)
       where
import Existential
import Data.Maybe
import ErrorMonad
import Data.Array
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Functor
import Control.Monad hiding (forM,forM_,sequence,sequence_,msum,mapM,mapM_)
import Data.Monoid
import Control.Monad.State 
import Data.List
import Data.Complex
import Data.Word
import Data.Bits

impossible :: a
impossible = error "Impossible!"

impossibleM :: Monad m => m a
impossibleM = fail "Impossible!"

getState :: MonadState s m => m s
getState = get

type Integer = Word32