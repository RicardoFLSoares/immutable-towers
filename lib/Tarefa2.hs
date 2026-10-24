{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425
import Data.List


-- | Função que calcula os inimigos que estão ao alcance de uma dada torre.
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance t (x:xs)
    | distancia (posicaoTorre t) (posicaoInimigo x) <= alcanceTorre t = x : inimigosNoAlcance t xs
    | otherwise = inimigosNoAlcance t xs
    where 
        distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2) 

-- | Função que atualiza o estado de um inimigo ao ser atingido por um projétil de uma torre.
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo t i = i {
    vidaInimigo = max 0 (vidaInimigo i - danoTorre t),
    projeteisInimigo = atualizaProjeteis (projetilTorre t) (projeteisInimigo i)
  }

-- | Função que atualiza a lista de projéteis que um inimigo transporta após ser atingido por um projétil.
atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis p [] = [p]
atualizaProjeteis p ps
  -- Cancelamento mútuo de Gelo e Fogo
  | tipoProjetil p == Gelo && any ((== Fogo) . tipoProjetil) ps = filter (\x -> tipoProjetil x `notElem` [Gelo, Fogo]) ps
  | tipoProjetil p == Fogo && any ((== Gelo) . tipoProjetil) ps = filter (\x -> tipoProjetil x `notElem` [Gelo, Fogo]) ps
  -- Resina dobra a duração do Fogo
  | tipoProjetil p == Resina && any ((== Fogo) . tipoProjetil) ps = map duplicaFogo (filter ((/= Resina) . tipoProjetil) ps)
  | tipoProjetil p == Fogo && any ((== Resina) . tipoProjetil) ps = map duplicaFogo (filter ((/= Resina) . tipoProjetil) ps)
  -- Caso contrário, soma os projéteis de forma adequada
  | otherwise = somaProjetil p ps

-- | Função que duplica a duração de um projétil de Fogo.
duplicaFogo :: Projetil -> Projetil
duplicaFogo proj
  | tipoProjetil proj == Fogo = proj { duracaoProjetil = duplicaDuracao (duracaoProjetil proj) }
  | otherwise = proj

-- | Duplica a duração de um projétil.
duplicaDuracao :: Duracao -> Duracao
duplicaDuracao (Finita d) = Finita (2 * d)
duplicaDuracao Infinita = Infinita


-- | Soma a duração de projéteis iguais.
somaProjetil :: Projetil -> [Projetil] -> [Projetil]
somaProjetil novoProj projeteis =
  let (iguais, diferentes) = partition (\p -> tipoProjetil p == tipoProjetil novoProj) projeteis
      duracaoTotal = somaDuracoes (duracaoProjetil novoProj) (map duracaoProjetil iguais)
  in if duracaoTotal > Finita 0
     then (novoProj { duracaoProjetil = duracaoTotal }) : diferentes
     else diferentes  

-- Soma as durações de vários projéteis
somaDuracoes :: Duracao -> [Duracao] -> Duracao
somaDuracoes base [] = base
somaDuracoes (Finita d1) (Finita d2 : ds) = somaDuracoes (Finita (d1 + d2)) ds
somaDuracoes Infinita _ = Infinita
somaDuracoes _ (Infinita : _) = Infinita



-- | Função que ativa um inimigo a partir de um portal.
--   Se o portal contiver ondas, o primeiro inimigo da onda é removido e ativado.
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos 
        | null (ondasPortal portal) = (portal, inimigosAtivos) -- Se não houver ondas no portal, não há inimigos ativos
        | null (inimigosOnda primeiraOnda) = (portal {ondasPortal = restoOndas}, inimigosAtivos) -- Onda Vazia
        | otherwise = (novoPortal, novosInimigos : inimigosAtivos)
        where 
            primeiraOnda = head ( ondasPortal portal)
            restoOndas = tail (ondasPortal portal)
            novoPortal = portal {ondasPortal = novaOnda : restoOndas} 
            novosInimigos = head ( inimigosOnda primeiraOnda)
            novaOnda = primeiraOnda { inimigosOnda = tail (inimigosOnda primeiraOnda) }

-- | Verifica se o jogo terminou.
--   O jogo termina se o jogador ganhar ou perder.
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo


-- | Verifica se o jogador ganhou o jogo.
--   O jogador ganha se não houver mais inimigos no mapa ou nos portais, e a base ainda tiver vida.
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && null inimigosPortais && vidaBase (baseJogo jogo) > 0
  where
    -- Obtemos os inimigos dentro dos portais
    inimigosPortais = concatMap (concatMap inimigosOnda . ondasPortal) (portaisJogo jogo)
    
-- | Verifica se o jogador perdeu o jogo.
--   O jogador perde se a vida da base for menor ou igual a zero.
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0


-------------------------------------


-- Exemplo Torre
t1 = Torre {
            posicaoTorre = (0.5,5.5),
            danoTorre = 5,
            alcanceTorre = 2,
            rajadaTorre = 2,
            cicloTorre = 1, 
            tempoTorre = 4, 
            projetilTorre = Projetil { 
                                tipoProjetil = Gelo,
                                duracaoProjetil = Finita 5
                            }
        }
-- Exemplos de inimigos
i1 = Inimigo {
            posicaoInimigo = (1.5,0.5),
            direcaoInimigo = Sul, 
            vidaInimigo = 20,
            velocidadeInimigo = 1,
            ataqueInimigo = 5,
            butimInimigo = 5,
            projeteisInimigo = [ 
                Projetil {
                        tipoProjetil = Fogo,
                        duracaoProjetil = Finita 1 
                },
                Projetil {
                        tipoProjetil = Resina,
                        duracaoProjetil = Infinita
                }
            ] 
        }
i2 = Inimigo {
            posicaoInimigo = (1.5,5.5),
            direcaoInimigo = Este, 
            vidaInimigo = 20,
            velocidadeInimigo = 1,
            ataqueInimigo = 5,
            butimInimigo = 5,
            projeteisInimigo = [ 
                Projetil {
                        tipoProjetil = Fogo,
                        duracaoProjetil = Finita 1 
                },
                Projetil {
                        tipoProjetil = Resina,
                        duracaoProjetil = Infinita
                }
            ] 
        }

-- Jogo para testar as funções
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
                            ],
                            cicloOnda = 5,
                            tempoOnda = 5,
                            entradaOnda =5 

                        
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
            posicaoInimigo = (0.5,0.5),
            direcaoInimigo = Este, 
            vidaInimigo = 20,
            velocidadeInimigo = 1,
            ataqueInimigo = 5,
            butimInimigo = 5,
            projeteisInimigo = [] 
        },
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