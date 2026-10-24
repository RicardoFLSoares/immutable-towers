{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

import Data.List 


-- | Função principal que valida o estado do jogo.
-- Verifica se todos os componentes (portais, inimigos, torres e base) respeitam as regras do jogo.
validaJogo :: Jogo -> Bool
validaJogo jogo =
    validaPortais jogo &&
    validaInimigos jogo &&
    validaTorres jogo &&
    validaBase jogo



-- | Função utilizada para validar os portais do jogo.
validaPortais :: Jogo -> Bool
validaPortais jogo = aux1 (portaisJogo jogo) && -- Verifica se há pelo menos um portal.
                     aux2 (portaisJogo jogo) jogo && -- Verifica se todos os portais estão sobre terrenos do tipo Terra.
                     aux3 jogo && -- Verifica se há um caminho válido do portal até à base.
                     aux4 jogo && -- Verifica que os portais não estão sobrepostos com a base ou torres.
                     aux5 jogo -- Verifica que cada portal tem no máximo uma onda de inimigos.
  where
    aux1 l = not (null l)
    aux2 [] _ = True
    aux2 (h:t) l = sobreTerra (posicaoPortal h) l && aux2 t l
    aux3 jogo = all (\portal -> caminhoParaBase (posicaoPortal portal) (posicaoBase (baseJogo jogo)) (mapaJogo jogo) Nothing) (portaisJogo jogo)
      where
        caminhoParaBase inicio fim mapa ultimaPosicao
            | inicio == fim = True
            | not (ehTerra inicio mapa) = False
            | otherwise = any (\v -> caminhoParaBase v fim mapa (Just inicio)) (vizinhosValidos inicio mapa ultimaPosicao)

        vizinhosValidos (x, y) mapa ultimaPosicao =
            filter (\(vx, vy) -> ehTerra (vx, vy) mapa && Just (vx, vy) /= ultimaPosicao) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

        ehTerra (x, y) mapa = let linha = floor y; coluna = floor x in
                              linha >= 0 && linha < length mapa &&
                              coluna >= 0 && coluna < length (head mapa) &&
                              (mapa !! linha !! coluna == Terra)
    aux4 jogo = all naoSobreposto (portaisJogo jogo)
      where
        listapos = posicaoBase (baseJogo jogo) : map posicaoTorre (torresJogo jogo)
        naoSobreposto portal = posicaoPortal portal `notElem` listapos
    aux5 jogo = all (\portal -> length (ondasPortal portal) <= 1) (portaisJogo jogo)

-- | Função utilizada para validar os inimigos do jogo.
validaInimigos :: Jogo -> Bool
validaInimigos jogo = aux1 jogo &&  -- Os inimigos começam nos portais.
                      aux2 (inimigosJogo jogo) jogo &&  -- Os inimigos estão sobre Terra.
                      aux3 (inimigosJogo jogo) jogo &&  -- Os inimigos não estão sobre torres.
                      aux4 (inimigosJogo jogo) &&  -- A velocidade dos inimigos é não negativa.
                      aux5 jogo  -- Os projéteis transportados pelos inimigos são válidos.
  where
    aux1 jogo = all verificaInimigo inimigosPortais
      where
        inimigosPortais = concatMap extrairInimigos (portaisJogo jogo)
        extrairInimigos portal = concatMap inimigosOnda (ondasPortal portal)
        verificaInimigo inimigo =
          posicaoInimigo inimigo `elem` map posicaoPortal (portaisJogo jogo) &&
          vidaInimigo inimigo > 0 &&
          null (projeteisInimigo inimigo)
    aux2 [] _ = True
    aux2 (j:jt) jogo = sobreTerra (posicaoInimigo j) jogo && aux2 jt jogo
    aux3 [] _ = True
    aux3 (j:jt) jogo = posicaoInimigo j `notElem` map posicaoTorre (torresJogo jogo) && aux3 jt jogo
    aux4 [] = True
    aux4 (h:t) = velocidadeInimigo h >= 0 && aux4 t
    aux5 jogo = all verificaInimigo (inimigosJogo jogo)
      where
        verificaInimigo inimigo =
          let projeteis = projeteisInimigo inimigo
              tipos = map tipoProjetil projeteis
              tiposUnicos = nub tipos
              tiposValidos = length tipos == length tiposUnicos
              tiposIncompatíveis = any (\xs -> all (`elem` tipos) xs) [[Fogo, Resina], [Fogo, Gelo]]
          in tiposValidos && not tiposIncompatíveis


                    
-- | Função utilizada para validar as torres do jogo.
validaTorres :: Jogo -> Bool
validaTorres jogo = aux1 (torresJogo jogo) jogo &&  -- As torres estão sobre Relva.
                    aux2 (torresJogo jogo) &&       -- As torres têm alcance positivo.
                    aux3 (torresJogo jogo) &&       -- As torres têm rajada maior que zero.
                    aux4 (torresJogo jogo) &&       -- As torres têm ciclo não negativo.
                    aux5 (torresJogo jogo)          -- As torres não estão sobrepostas.
    where
        aux1 [] _ = True
        aux1 (h:t) jogo = sobreRelva (posicaoTorre h) jogo && aux1 t jogo
        aux2 [] = True 
        aux2 (h:t) = alcanceTorre h > 0 && aux2 t 
        aux3 [] = True 
        aux3 (h:t) = rajadaTorre h > 0 && aux3 t 
        aux4 [] = True 
        aux4 (h:t) = cicloTorre h >= 0 && aux4 t 
        aux5 torres =
            let posicoes = map posicaoTorre torres
            in length posicoes == length (nub posicoes)

-- | Função utilizada para validar a base do jogo.
validaBase :: Jogo -> Bool
validaBase jogo = aux1 (baseJogo jogo) jogo &&  -- A base está sobre Terra.
                  aux2 (baseJogo jogo) &&       -- Os créditos da base são não negativos.
                  aux3 (baseJogo jogo) jogo     -- A base não está sobre portais ou torres.
    where
        aux1 base jogo = sobreTerra (posicaoBase base) jogo
        aux2 base = creditosBase base >= 0 
        aux3 base jogo = posicaoBase base `notElem` (map posicaoPortal (portaisJogo jogo) ++ map posicaoTorre (torresJogo jogo))


-- | Função auxiliar que verifica se uma posição está sobre uma célula de terreno do tipo Terra no mapa do jogo.
sobreTerra :: Posicao -> Jogo -> Bool 
sobreTerra (x,y) jogo 
       | linha >= 0 && linha < length mapa && coluna >= 0 && coluna < length (head mapa) && (mapa !! linha) !! coluna == Terra = True
       | otherwise = False  
        where   mapa   = mapaJogo jogo 
                linha  = floor y 
                coluna = floor x

-- | Função auxiliar que verifica se uma posição está sobre uma célula de terreno do tipo Relva no mapa do jogo.              
sobreRelva :: Posicao -> Jogo -> Bool
sobreRelva (x,y) jogo 
       | linha >= 0 && linha < length mapa && coluna >= 0 && coluna < length (head mapa) && (mapa !! linha) !! coluna == Relva = True
       | otherwise = False  
        where   mapa   = mapaJogo jogo 
                linha  = floor y 
                coluna = floor x



-- | Estado de teste do jogo para validar as funções de verificação.
jogoTeste :: Jogo
jogoTeste = Jogo {
    baseJogo = Base {
        vidaBase = 100 , 
        posicaoBase = (4.5,5.5), 
        creditosBase = 20
    },
    portaisJogo = 
        [
            Portal 
                {
                posicaoPortal = (0.5,0.5), 
                ondasPortal = 
                    [
                        Onda {
                           inimigosOnda = [ 
                                    Inimigo {
                                        posicaoInimigo = (0.5,0.5),
                                        direcaoInimigo = Este, 
                                        vidaInimigo = 20,
                                        velocidadeInimigo = 1,
                                        ataqueInimigo = 5,
                                        butimInimigo = 5,
                                        projeteisInimigo = [] 
                                    }
                            ]
                        }
                    ]
                }
        ],
    torresJogo = [
        Torre {
            posicaoTorre = (0.5,5.5),
            danoTorre = 5,
            alcanceTorre = 270,
            rajadaTorre = 2,
            cicloTorre = 1, 
            tempoTorre = 4, 
            projetilTorre = Projetil { 
                                tipoProjetil = Fogo,
                                duracaoProjetil = Finita 2 
                            }
        },
        Torre {
            posicaoTorre = (2.5,0.5),
            danoTorre = 5,
            alcanceTorre = 270,
            rajadaTorre = 2,
            cicloTorre = 1, 
            tempoTorre = 4, 
            projetilTorre = Projetil { 
                                tipoProjetil = Resina,
                                duracaoProjetil = Infinita
                            }
        }
    ],
    mapaJogo     = let r = Relva 
                       t = Terra
                       a = Agua  
                    in
                     [[t, t, r, r, a, r],
                      [r, t, a, a, a, r],
                      [t, t, a, r, r, r],
                      [t, a, a, r, r, r],
                      [t, t, r, r, r, r],
                      [r, t, t, t, t, r]],

    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (1.5,2.5),
            direcaoInimigo = Oeste, 
            vidaInimigo = 20,
            velocidadeInimigo = 1,
            ataqueInimigo = 5,
            butimInimigo = 5,
            projeteisInimigo = [ 
                Projetil {
                        tipoProjetil = Fogo,
                        duracaoProjetil = Finita 1 
                }
            ] 
        },
        Inimigo {
            posicaoInimigo = (0.5,2.5),
            direcaoInimigo = Oeste, 
            vidaInimigo = 20,
            velocidadeInimigo = 1,
            ataqueInimigo = 5,
            butimInimigo = 5,
            projeteisInimigo = [ 
                Projetil {
                        tipoProjetil = Fogo,
                        duracaoProjetil = Finita 1 
                }
            ] 
        }
    ], 
    lojaJogo = [ (  20, 
                    Torre {
                        posicaoTorre = (2.5,0.5),
                        danoTorre = 5,
                        alcanceTorre = 270,
                        rajadaTorre = 2,
                        cicloTorre = 1, 
                        tempoTorre = 4, 
                        projetilTorre = Projetil { 
                                            tipoProjetil = Fogo,
                                            duracaoProjetil = Finita 2
                                        }
                    }
                  ),
                  ( 15,
                    Torre {
                        posicaoTorre = (4.5,2.5),
                        danoTorre = 3,
                        alcanceTorre = 270,
                        rajadaTorre = 2,
                        cicloTorre = 1, 
                        tempoTorre = 4, 
                        projetilTorre = Projetil { 
                                            tipoProjetil = Resina,
                                            duracaoProjetil = Infinita
                                        }
                    }
                  )
                ]

               
} 
