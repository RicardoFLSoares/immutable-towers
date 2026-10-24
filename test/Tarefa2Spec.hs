module Tarefa2Spec (testesTarefa2) where

import Tarefa2
import Test.HUnit
import LI12425

testesTarefa2 :: Test
testesTarefa2 = TestList
  [ TestLabel "Teste 1 - Inimigos No Alcance" teste1,
    TestLabel "Teste 2 - Atinge Inimigo" teste2,
    TestLabel "Teste 3 - Ativa Inimigo" teste3,
    TestLabel "Teste 4 - Terminou Jogo" teste4
  ]


-- Teste 1: inimigosNoAlcance
teste1 = TestCase $ do
  assertEqual "Nenhum inimigo no alcance" [] (inimigosNoAlcance torre1 [])
  assertEqual "Inimigos dentro e fora do alcance" [inimigo1] (inimigosNoAlcance torre1 [inimigo1, inimigoFora])
  where
    torre1 = Torre (0, 0) 10 3 1 1 0 (Projetil Fogo (Finita 1))
    inimigo1 = Inimigo (1, 1) Norte 10 1 1 1 []
    inimigoFora = Inimigo (5, 5) Norte 10 1 1 1 []

-- Teste 2: atingeInimigo
teste2 = TestCase $ do
  let esperado = inimigo { vidaInimigo = 5, projeteisInimigo = [Projetil Fogo (Finita 2)] }
  assertEqual "Inimigo atingido por torre de fogo" esperado (atingeInimigo torreFogo inimigo)
  where
    torreFogo = Torre (0, 0) 5 3 1 1 0 (Projetil Fogo (Finita 2))
    inimigo = Inimigo (1, 1) Norte 10 1 1 1 []

-- Teste 3: ativaInimigo
teste3 = TestCase $ do
  assertEqual "Ativação de inimigos de um portal" esperado (snd (ativaInimigo portal1 []))
  where
    portal1 = Portal (0, 0) [onda1]
    onda1 = Onda [inimigo1, inimigo2] 1.0 1.0 1.0
    inimigo1 = Inimigo (0, 0) Norte 10 1 1 1 []
    inimigo2 = Inimigo (0, 0) Norte 10 1 1 1 []
    esperado = [inimigo1]

-- Teste 4: terminouJogo
teste4 = TestCase $ do
  assertBool "Jogo não terminou" (not (terminouJogo jogo1))
  assertBool "Jogo ganho" (ganhouJogo jogo2)
  assertBool "Jogo perdido" (perdeuJogo jogo3)
  where
    jogo1 = Jogo (Base 10 (0, 0) 10) [] [] [] [inimigo1] []
    jogo2 = Jogo (Base 10 (0, 0) 10) [] [] [] [] []
    jogo3 = Jogo (Base 0 (0, 0) 10) [] [] [] [] []
    inimigo1 = Inimigo { vidaInimigo = 5, posicaoInimigo = (1, 1), direcaoInimigo = Norte }

-- Função para executar os testes unitários
executaTestesTarefa2 :: IO ()
executaTestesTarefa2 = runTestTT testesTarefa2 >> return ()