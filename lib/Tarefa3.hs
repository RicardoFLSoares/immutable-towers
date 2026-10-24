{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}

module Tarefa3 where

import LI12425
import Tarefa2
import Data.List

-- | Atualiza o estado do jogo em função do tempo decorrido.
--   Esta função é a principal responsável por coordenar a atualização dos portais, torres e inimigos.
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo =
  let
    -- Primeiro, atualizamos os portais para ativar novos inimigos.
    jogoComPortais = atualizaPortais tempo jogo

    -- Depois, atualizamos o estado das torres e dos inimigos.
    (torresAtualizadas, inimigosAtualizados) =
      atualizaTorres tempo (torresJogo jogoComPortais) (inimigosJogo jogoComPortais)

    -- Finalmente, atualizamos os inimigos no mapa.
    jogoFinal = jogoComPortais { inimigosJogo = inimigosAtualizados }
  in
    atualizaInimigos tempo jogoFinal { torresJogo = torresAtualizadas }


-- | Atualiza todas as torres do jogo, considerando o tempo decorrido e os inimigos no mapa.
atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres _ [] inimigos = ([], inimigos)
atualizaTorres tempo (torre:t) inimigos =
  let (torreAtualizada, inimigosAtualizados) = atualizaTorre tempo torre inimigos
      (torresRestantes, inimigosFinal) = atualizaTorres tempo t inimigosAtualizados
  in (torreAtualizada : torresRestantes, inimigosFinal)


-- | Atualiza o estado de uma torre, verificando se ela dispara nos inimigos ao alcance.
atualizaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
atualizaTorre tempo torre inimigos
  | tempoTorre torre > 0 = (torre { tempoTorre = max 0 (tempoTorre torre - tempo) }, inimigos)
  | otherwise =
      let inimigosNoAlcance = filtrarInimigosNoAlcance (posicaoTorre torre) (alcanceTorre torre) inimigos
          inimigosAtingidos = take (rajadaTorre torre) inimigosNoAlcance
          inimigosRestantes = atualizaInimigosAtingidos torre inimigosAtingidos inimigos
          novoTempoTorre = if null inimigosAtingidos then 0 else cicloTorre torre
      in (torre { tempoTorre = novoTempoTorre }, inimigosRestantes)


-- | Filtra os inimigos que estão dentro do alcance de uma torre.
filtrarInimigosNoAlcance :: Posicao -> Float -> [Inimigo] -> [Inimigo]
filtrarInimigosNoAlcance posTorre alcance = filter (\i -> distancia posTorre (posicaoInimigo i) <= alcance)

-- | Atualiza o estado dos inimigos atingidos por uma torre.
atualizaInimigosAtingidos :: Torre -> [Inimigo] -> [Inimigo] -> [Inimigo]
atualizaInimigosAtingidos torre atingidos todos =
  let inimigosAtualizados = map (atingeInimigo torre) atingidos
  in map (\i -> if posicaoInimigo i `elem` map posicaoInimigo inimigosAtualizados
                then head (filter (\ia -> posicaoInimigo ia == posicaoInimigo i) inimigosAtualizados)
                else i) todos

-- | Calcula a distância euclidiana entre duas posições.
distancia :: Posicao -> Posicao -> Float 
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- | Movimenta todos os inimigos no mapa.
moverInimigos :: Tempo -> Mapa -> Posicao -> [Inimigo] -> [Inimigo]
moverInimigos dt mapa posBase inimigos =
  map (\inimigo -> moverInimigo dt mapa posBase inimigo) inimigos

-- | Movimenta um único inimigo no mapa, considerando o tempo decorrido e a posição da base.
moverInimigo :: Tempo -> Mapa -> Posicao -> Inimigo -> Inimigo
moverInimigo tempo mapa posBase inimigo
  | distancia posBase (posicaoInimigo inimigo) < 0.2 = inimigo { posicaoInimigo = posBase } -- Chegou à base
  | sobreTerra (novaPosicao 0.51) mapa = inimigo { posicaoInimigo = moveParaOutro (posicaoInimigo inimigo) (novaPosicao 0.5) velocidade tempo }
  | otherwise =
      let novaDirecao = encontrarNovaDirecao mapa (posicaoInimigo inimigo) (direcaoInimigo inimigo)
      in inimigo { direcaoInimigo = novaDirecao }
  where
    velocidade = velocidadeInimigo inimigo
    novaPosicao d = atualizaPosicao (posicaoInimigo inimigo) d (direcaoInimigo inimigo)

-- | Encontra uma nova direção se o inimigo encontrar Relva ou Água.
encontrarNovaDirecao :: Mapa -> Posicao -> Direcao -> Direcao
encontrarNovaDirecao mapa posicao direcaoAtual
  | sobreTerra (atualizaPosicao posicao 0.51 dirDireita) mapa = dirDireita
  | sobreTerra (atualizaPosicao posicao 0.51 dirEsquerda) mapa = dirEsquerda
  | otherwise = direcaoAtual
  where
    dirDireita = viraDireita direcaoAtual
    dirEsquerda = viraEsquerda direcaoAtual

-- | Movimenta uma posição em direção a outra.
moveParaOutro :: Posicao -> Posicao -> Float -> Tempo -> Posicao
moveParaOutro (xOrigem, yOrigem) (xDestino, yDestino) velocidade tempo
    | distancia (xOrigem, yOrigem) (xDestino, yDestino) <= 0.01 = (xDestino, yDestino)
    | otherwise = (xOrigem + dx * deslocamento, yOrigem + dy * deslocamento)
  where
    dx = (xDestino - xOrigem) / distancia (xOrigem, yOrigem) (xDestino, yDestino)
    dy = (yDestino - yOrigem) / distancia (xOrigem, yOrigem) (xDestino, yDestino)
    deslocamento = velocidade * tempo

-- | Calcula a posição contínua ao longo de uma direção.
atualizaPosicao :: Posicao -> Float -> Direcao -> Posicao
atualizaPosicao (x, y) d Norte = (x, y - d)
atualizaPosicao (x, y) d Sul   = (x, y + d)
atualizaPosicao (x, y) d Este  = (x + d, y)
atualizaPosicao (x, y) d Oeste = (x - d, y)

-- | Verifica se a posição é um terreno de terra.
sobreTerra :: Posicao -> Mapa -> Bool
sobreTerra (x, y) mapa 
       | linha >= 0 && linha < length mapa && coluna >= 0 && coluna < length (head mapa) && (mapa !! linha) !! coluna == Terra = True
       | otherwise = False  
        where   linha  = floor y 
                coluna = floor x

-- | Gira a direção de um inimigo 90 graus para a direita.
--   Utilizado quando o inimigo precisa de mudar de direção para evitar terrenos intransitáveis (como Relva ou Água).
viraDireita :: Direcao -> Direcao
viraDireita Norte = Este
viraDireita Este  = Sul
viraDireita Sul   = Oeste
viraDireita Oeste = Norte

-- | Gira a direção de um inimigo 90 graus para a esquerda.
--   Utilizado quando o inimigo precisa de mudar de direção para evitar terrenos intransitáveis (como Relva ou Água).
viraEsquerda :: Direcao -> Direcao
viraEsquerda Norte = Oeste
viraEsquerda Oeste = Sul
viraEsquerda Sul   = Este
viraEsquerda Este  = Norte


-- | Atualiza o estado dos inimigos no jogo, considerando o tempo decorrido.
--   Movimenta os inimigos, aplica efeitos contínuos (como Fogo, Gelo e Resina), e verifica quais inimigos chegaram à base,
--   quais morreram, e como isso afeta o estado da base do jogador (vida e créditos).
atualizaInimigos :: Tempo -> Jogo -> Jogo
atualizaInimigos dt jogo = jogo {
    inimigosJogo = novosInimigos,     -- Atualiza a lista de inimigos vivos no jogo.
    baseJogo = baseAtualizada         -- Atualiza o estado da base (vida e créditos).
  }
  where
    -- Obtém o mapa do jogo e a posição da base.
    mapa = mapaJogo jogo
    posBase = posicaoBase (baseJogo jogo)

    -- Lista atual de inimigos no jogo.
    inimigosAtuais = inimigosJogo jogo

    -- Movimenta os inimigos e aplica efeitos contínuos (como redução de velocidade e dano contínuo).
    inimigosMovidos = map (atualizaProjetilInimigo dt . aplicaEfeitos dt) (moverInimigos dt mapa posBase inimigosAtuais)

    -- Separa os inimigos que chegaram à base dos que ainda estão no mapa.
    (inimigosNoMapa, inimigosNaBase) = partition (\i -> posicaoInimigo i /= posBase) inimigosMovidos

    -- Filtra os inimigos mortos para calcular o butim ganho pelo jogador.
    inimigosMortos = filter morreu inimigosMovidos

    -- Atualiza o estado da base:
    -- - Reduz a vida da base pelo ataque dos inimigos que chegaram à base.
    -- - Aumenta os créditos da base pelo butim ganho dos inimigos mortos.
    baseAtualizada = (baseJogo jogo) {
        vidaBase = max 0 (vidaBase (baseJogo jogo) - sum (map ataqueInimigo inimigosNaBase)),
        creditosBase = creditosBase (baseJogo jogo) + sum (map butimInimigo inimigosMortos)
      }

    -- Filtra apenas os inimigos vivos para a próxima iteração.
    novosInimigos = filter (not . morreu) inimigosNoMapa

    -- Verifica se um inimigo morreu (vida menor ou igual a 0).
    morreu inimigo = vidaInimigo inimigo <= 0

-- | Função que atualiza a duração dos projéteis ativos no inimigo
atualizaProjetilInimigo :: Tempo -> Inimigo -> Inimigo
atualizaProjetilInimigo dt inimigo =
  inimigo { projeteisInimigo = filter (not . expirou) (map (reduzDuracao dt) (projeteisInimigo inimigo)) }

-- | função que reduz a duração de um projétil
reduzDuracao :: Tempo -> Projetil -> Projetil
reduzDuracao dt (Projetil tipo (Finita duracao)) = Projetil tipo (Finita (duracao - dt))
reduzDuracao _ projetil@(Projetil _ Infinita) = projetil

-- | Função que verifica se o projétil expirou
expirou :: Projetil -> Bool
expirou (Projetil _ (Finita duracao)) = duracao <= 0
expirou (Projetil _ Infinita) = False

-- | Função que aplica efeitos contínuos nos inimigos
aplicaEfeitos :: Tempo -> Inimigo -> Inimigo
aplicaEfeitos dt inimigo =
  let efeitos = projeteisInimigo inimigo
  in inimigo
      { velocidadeInimigo = atualizaVelocidade (velocidadeInimigo inimigo) efeitos,
        vidaInimigo = max 0 (vidaInimigo inimigo - danoFogo efeitos * dt)
      }

-- | Funão que atualiza a velocidade de um inimigo com base nos efeitos que ele está a sofrer.
atualizaVelocidade :: Float -> [Projetil] -> Float
atualizaVelocidade velocidade efeitos
  | temGelo efeitos = 0  -- Gelo impede o movimento
  | temResina efeitos && velocidade == velocidadeOriginal = velocidade / 2  -- Aplica Resina uma vez
  | otherwise = velocidadeOriginal  -- Mantém a velocidade atual

-- | Velocidade original do inimigo
velocidadeOriginal :: Float
velocidadeOriginal = 0.5

-- | Calcula o dano contínuo causado por Fogo
danoFogo :: [Projetil] -> Float
danoFogo efeitos = sum [4 | p <- efeitos, tipoProjetil p == Fogo, not (expirou p)]

-- | Função que verifica se o inimigo tem um projétil de Gelo
temGelo :: [Projetil] -> Bool
temGelo = any (\p -> tipoProjetil p == Gelo && not (expirou p))

-- | Função que verifica se o inimigo tem um projétil de Fogo
temFogo :: [Projetil] -> Bool
temFogo = any (\p -> tipoProjetil p == Fogo && not (expirou p))

-- | Função que verifica se o inimigo tem um projétil de Resina
temResina :: [Projetil] -> Bool
temResina = any (\p -> tipoProjetil p == Resina && not (expirou p))

-- | Função que atualiza o estado dos portais no jogo, ativando inimigos de acordo com o tempo decorrido.
--   Esta função é responsável por lançar novos inimigos a partir dos portais e adicioná-los à lista de inimigos ativos no jogo.
atualizaPortais :: Tempo -> Jogo -> Jogo
atualizaPortais dt jogo = jogo {
    portaisJogo = novosPortais,
    inimigosJogo = inimigosJogo jogo ++ novosInimigos
  }
  where
    (novosPortais, novosInimigosListas) = unzip (map (atualizaPortal dt) (portaisJogo jogo))
    novosInimigos = concat novosInimigosListas

-- | Função que atualiza o estado de um portal, lançando inimigos das ondas de acordo com o tempo decorrido.
atualizaPortal :: Tempo -> Portal -> (Portal, [Inimigo])
atualizaPortal dt portal =
  let 
    -- Atualiza todas as ondas de inimigos associadas ao portal.
    (novasOndas, inimigosLancados) = unzip (map (atualizaOnda dt) (ondasPortal portal))
  in 
    -- Retorna o portal atualizado e a lista de inimigos lançados.
    (portal { ondasPortal = novasOndas }, concat inimigosLancados)


-- | Função que atualiza o estado de uma onda de inimigos, verificando se está pronta para lançar novos inimigos.
atualizaOnda :: Tempo -> Onda -> (Onda, [Inimigo])
atualizaOnda dt onda
  | entradaOnda onda > 0 = (onda { entradaOnda = entradaOnda onda - dt }, [])  -- Aguarda pelo tempo de entrada da onda.
  | tempoOnda onda > 0 = (onda { tempoOnda = tempoOnda onda - dt }, [])        -- Aguarda pelo tempo de ciclo da onda.
  | null (inimigosOnda onda) = (onda, [])                                      -- Se não há mais inimigos na onda, nada é lançado.
  | otherwise =
      -- Lança o primeiro inimigo da lista da onda.
      let inimigoLancar = head (inimigosOnda onda)
          inimigosRestantes = tail (inimigosOnda onda)
          novaOnda = onda { inimigosOnda = inimigosRestantes, tempoOnda = cicloOnda onda }
      in (novaOnda, [inimigoLancar])  -- Retorna a onda atualizada e o inimigo lançado.
