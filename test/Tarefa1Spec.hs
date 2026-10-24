
module Tarefa1Spec (testesTarefa1) where

import Tarefa1
import Test.HUnit


testesTarefa1 :: Test
testesTarefa1 = TestList
  [ TestLabel "Teste 1 - validaJogo" teste1,
    TestLabel "Teste 2 - validaPortais" teste2,
    TestLabel "Teste 3 - validaInimigos" teste3,
    TestLabel "Teste 4 - validaTorres" teste4,
    TestLabel "Teste 5 - validaBase" teste5
  ]

teste1 = TestCase $ assertBool "validaJogo - Caso Base" (validaJogo jogoTeste)
teste2 = TestCase $ assertBool "validaPortais - Portais Sobre Terra" (validaPortais jogoTeste)
teste3 = TestCase $ assertBool "validaInimigos - Inimigos em Posição Válida" (validaInimigos jogoTeste)
teste4 = TestCase $ assertBool "validaTorres - Torres com Alcance e Ciclo Válidos" (validaTorres jogoTeste)
teste5 = TestCase $ assertBool "validaBase - Base com Créditos Positivos" (validaBase jogoTeste)

-- Função para executar os testes unitários
executaTestesTarefa1 :: IO ()
executaTestesTarefa1 = runTestTT testesTarefa1 >> return ()