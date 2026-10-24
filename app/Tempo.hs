{-|
Module      : Tempo
Description : Gestão da evolução temporal do jogo "Immutable Towers"
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>

O módulo `Tempo` contém funções responsáveis por atualizar o estado do jogo em função do tempo decorrido.  
A função principal `reageTempo` recebe o tempo passado desde o último frame (`dt`) e o estado atual do jogo (`ImmutableTowers`), devolvendo o novo estado do jogo.  
Este módulo é fundamental para a evolução contínua do jogo, como o movimento dos inimigos, ataques das torres e verificação das condições de vitória ou derrota.
-}

module Tempo where

import ImmutableTowers
import Tarefa3
import Tarefa2

-- | A função `reageTempo` é responsável por atualizar o estado do jogo com base no tempo decorrido.
--   Recebe um valor de tempo (`dt`) em segundos e o estado atual do jogo (`ImmutableTowers`), e devolve o novo estado do jogo.
reageTempo :: Float             -- ^ O tempo decorrido desde o último frame (em segundos).
           -> ImmutableTowers   -- ^ O estado atual do jogo.
           -> ImmutableTowers   -- ^ O estado do jogo após a atualização.

reageTempo dt game =
  case gameState game of
    -- Quando o jogo está ativo, atualiza o estado do jogo com base no tempo decorrido.
    JogoAtivo jogo ->
      let jogoAtualizado = atualizaJogo dt jogo
      in if ganhouJogo jogoAtualizado
         -- Se o jogador atingir a condição de vitória, o estado do jogo muda para "Final" com a mensagem "Ganhou".
         then game { gameState = Final "Ganhou" }
         -- Se o jogador perder o jogo, o estado muda para "Final" com a mensagem "Perdeu".
         else if perdeuJogo jogoAtualizado
         then game { gameState = Final "Perdeu" }
         -- Caso contrário, o estado do jogo é atualizado para o novo estado do jogo ativo.
         else game { gameState = JogoAtivo jogoAtualizado }
    -- Se o jogo já estiver em estado final ou noutro estado, mantém-se inalterado.
    _ -> game




