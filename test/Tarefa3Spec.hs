module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

-- Testes principais para a Tarefa 3
testesTarefa3 :: Test
testesTarefa3 = TestList
  [ TestLabel "Teste 1 - Atualiza Torres" testeAtualizaTorres,
    TestLabel "Teste 2 - Atualiza Inimigos" testeAtualizaInimigos,
    TestLabel "Teste 3 - Atualiza Portais" testeAtualizaPortais
  ]


-- Teste 1: atualizaTorres
testeAtualizaTorres = TestCase $ do
  let (torresAtualizadas, inimigosAtualizados) = atualizaTorres 1 [torreFogo] [inimigo1]
  assertBool "Inimigo deve ser atingido pela torre" (vidaInimigo (head inimigosAtualizados) < vidaInimigo inimigo1)
  assertEqual "Tempo da torre deve ser atualizado após disparo" (cicloTorre torreFogo) (tempoTorre (head torresAtualizadas))


-- Teste 2: atualizaInimigos
testeAtualizaInimigos = TestCase $ do
  let jogoInicial = Jogo baseInicial [] [] mapaTeste [inimigoAMover] []
  let jogoAtualizado = atualizaInimigos 1 jogoInicial
  let inimigoAtualizado = head (inimigosJogo jogoAtualizado)
  assertBool "Inimigo deve se mover" (distancia (posicaoInimigo inimigoAtualizado) (posicaoInimigo inimigoAMover) > 0)

-- Teste 3: atualizaPortais
testeAtualizaPortais = TestCase $ do
  let jogoInicial = Jogo baseInicial [portal] [] mapaTeste [] []
  let jogoAtualizado = atualizaPortais 1 jogoInicial
  assertBool "Inimigo deve ser lançado pelo portal" (not (null (inimigosJogo jogoAtualizado)))



-- Dados do teste
baseInicial = Base { vidaBase = 100, creditosBase = 0, posicaoBase = (3.5, 2.5) }
portal = Portal { posicaoPortal = (0.5, 0.5), ondasPortal = [onda1] }
onda1 = Onda { entradaOnda = 0, tempoOnda = 0, cicloOnda = 5, inimigosOnda = [inimigo1] }
torreFogo = Torre { posicaoTorre = (2.5, 0.5), danoTorre = 5, alcanceTorre = 4, rajadaTorre = 2, cicloTorre = 2, tempoTorre = 0}
inimigo1 = Inimigo { posicaoInimigo = (1.5, 0.5), velocidadeInimigo = 1, vidaInimigo = 10, ataqueInimigo = 5, butimInimigo = 15, direcaoInimigo = Norte, projeteisInimigo = [] }
inimigoAMover = inimigo1 { posicaoInimigo = (1.5, 1.5), direcaoInimigo = Este }
mapaTeste = [[Terra, Terra, Relva, Relva], 
             [Relva, Terra, Terra, Terra], 
             [Relva, Relva, Relva, Terra]]

-- Função para executar os testes unitários
executaTestesTarefa3 :: IO ()
executaTestesTarefa3 = runTestTT testesTarefa3 >> return ()