{-# LANGUAGE DataKinds #-}

module Main where

import Data.Kind

type Nat :: Type
data Nat where
  Z :: Nat
  S :: Nat -> Nat

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

data Dir = In | Out

type Session :: Nat -> Type -> Type
data Session n t where
  SEnd :: Session n t
  SVar :: Fin n -> Session n t
  SCom :: Dir -> t -> Session n t
  SAlt :: Dir -> [Session n t] -> Session n t
  SMu :: Session (S n) t -> Session n t

main :: IO ()
main = putStrLn "Hello, Haskell!"
