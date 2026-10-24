{-|
Module      : ImmutableTowers
Description : Definição do estado do jogo "Immutable Towers"
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>

O módulo `ImmutableTowers` define os tipos de dados principais que representam o estado do jogo, incluindo o estado do menu, o estado do jogo em si, o mapa inicial e a torre selecionada. Estes tipos são fundamentais para a lógica interna e a gestão do progresso do jogo.
-}

module ImmutableTowers where

import LI12425 

-- | Tipo de dados que representa os possíveis estados do menu do jogo.
data MenuState
  = Start  -- ^ Estado inicial do menu principal.
  | Faq    -- ^ Estado em que o menu apresenta as FAQs do jogo.
  deriving (Eq, Show)

-- | Tipo de dados que representa os diferentes estados do jogo.
data GameState
  = Menu           -- ^ Estado em que o jogo está no menu principal.
  | EscolherNivel  -- ^ Estado em que o jogador escolhe um nível para jogar.
  | JogoAtivo Jogo -- ^ Estado em que o jogo está ativo, recebendo o estado atual do tipo `Jogo`.
  | Final String   -- ^ Estado final do jogo, que pode indicar vitória ou derrota.
  | Fq             -- ^ Estado que representa o menu FAQ.
  deriving (Show)

-- | Tipo de dados que representa o estado global do jogo "Immutable Towers".
data ImmutableTowers = ImmutableTowers
  { menuState :: MenuState        -- ^ O estado atual do menu.
  , gameState :: GameState        -- ^ O estado atual do jogo.
  , mapaInicial :: Mapa           -- ^ Mapa para fazer conversões
  , torreSelecionada :: Maybe String -- ^ A torre atualmente selecionada pelo jogador (se houver).
  }
  deriving (Show)


-- | Definição da torre de Fogo
torreFogo :: Torre
torreFogo = Torre
  { posicaoTorre = (0, 0),
    danoTorre = 40,
    alcanceTorre = 2.6,
    rajadaTorre = 2,
    cicloTorre = 5,
    tempoTorre = 0,
    projetilTorre = Projetil Fogo (Finita 8)
  }

-- | Definição da torre de Gelo
torreGelo :: Torre
torreGelo = Torre
  { posicaoTorre = (0, 0),
    danoTorre = 30,
    alcanceTorre = 2.1,
    rajadaTorre = 3,
    cicloTorre = 7,
    tempoTorre = 0,
    projetilTorre = Projetil Gelo (Finita 4)
  }

-- | Definição da torre de Resina
torreResina :: Torre
torreResina = Torre
  { posicaoTorre = (0, 0),
    danoTorre = 10,
    alcanceTorre = 2.1,
    rajadaTorre = 4,
    cicloTorre = 4,
    tempoTorre = 0,
    projetilTorre = Projetil Resina Infinita
  }




-- | Definição do jogo presente no nível 1 
jogo1 :: Jogo
jogo1 = Jogo
  { baseJogo = Base { vidaBase = 200, posicaoBase = (0.5, 4.5), creditosBase = 80 },
    portaisJogo =
      [ Portal
          { posicaoPortal = (0.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] }
                      ],
                    cicloOnda = 3,
                    tempoOnda = 2,
                    entradaOnda = 0
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 20, butimInimigo = 40, projeteisInimigo = [] }
                      ],
                    cicloOnda = 2,
                    tempoOnda = 2,
                    entradaOnda = 8
                  }
              ]
          }
      ],
    torresJogo = [],
    mapaJogo =
      [ [Terra, Terra, Terra, Terra, Relva, Relva],
        [Relva, Relva, Relva, Terra, Agua, Agua],
        [Relva, Agua, Agua, Terra, Agua, Relva],
        [Agua, Agua, Relva, Terra, Terra, Relva],
        [Terra, Relva, Relva, Relva, Terra, Relva],
        [Terra, Terra, Terra, Terra, Terra, Relva]
      ],
    inimigosJogo = [],
    lojaJogo =
      [ (30, torreFogo ),
        (20, torreGelo ),
        (20, torreResina )
      ]
  }




-- | Definição do jogo presente no nível 2
jogo2 :: Jogo
jogo2 = Jogo
  { baseJogo = Base { vidaBase = 200, posicaoBase = (0.5, 3.5), creditosBase = 50 },
    portaisJogo =
      [ Portal
          { posicaoPortal = (0.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] }
                      ],
                    cicloOnda = 3,
                    tempoOnda = 3,
                    entradaOnda = 0
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] }
                      ],
                    cicloOnda = 2.5,
                    tempoOnda = 0,
                    entradaOnda = 14
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 30, butimInimigo = 8, projeteisInimigo = [] }
                      ],
                    cicloOnda = 2,
                    tempoOnda = 2,
                    entradaOnda = 28
                  }
              ]
          }
      ],
    torresJogo = [],
    mapaJogo =
      [ [Terra, Terra, Relva, Relva, Agua, Agua],
        [Relva, Terra, Terra, Terra, Agua, Relva],
        [Agua, Agua, Agua, Terra, Agua, Relva],
        [Terra, Relva, Relva, Terra, Relva, Relva],
        [Terra, Relva, Relva, Terra, Relva, Relva],
        [Terra, Terra, Terra, Terra, Relva, Relva]
      ],
    inimigosJogo = [],
    lojaJogo =
      [ (30, torreFogo),
        (20, torreGelo),
        (20, torreResina)
      ]
  }




-- | Definição do jogo presente no nível 3
jogo3 :: Jogo
jogo3 = Jogo
  { baseJogo = Base { vidaBase = 100, posicaoBase = (5.5, 4.5), creditosBase = 100 },
    portaisJogo =
      [ Portal
          { posicaoPortal = (0.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] }
                      ],
                    cicloOnda = 3,
                    tempoOnda = 3,
                    entradaOnda = 0
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] }
                      ],
                    cicloOnda = 3,
                    tempoOnda = 0,
                    entradaOnda = 16
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 150, velocidadeInimigo = 0.5, ataqueInimigo = 50, butimInimigo = 5, projeteisInimigo = [] }
                      ],
                    cicloOnda = 1.5,
                    tempoOnda = 0,
                    entradaOnda = 35
                  }
              ]
          }
      ],
    torresJogo = [],
    mapaJogo =
      [ [Terra, Terra, Relva, Relva, Agua, Agua],
        [Relva, Terra, Terra, Terra, Agua, Relva],
        [Agua, Agua, Agua, Terra, Agua, Relva],
        [Relva, Relva, Relva, Terra, Relva, Relva],
        [Relva, Relva, Relva, Terra, Relva, Terra],
        [Relva, Relva, Relva, Terra, Terra, Terra]
      ],
    inimigosJogo = [],
    lojaJogo =
      [ (30, torreFogo ),
        (20, torreGelo ),
        (20, torreResina )
      ]
  }



-- | Definição do jogo presente no nível 4
jogo4 :: Jogo
jogo4 = Jogo
  { baseJogo = Base { vidaBase = 200, posicaoBase = (4.5, 5.5), creditosBase = 50 },
    portaisJogo =
      [ Portal
          { posicaoPortal = (0.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] }
                      ],
                    cicloOnda = 0.7,
                    tempoOnda = 2,
                    entradaOnda = 0
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (0.5, 0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] }
                        
                      ],
                    cicloOnda = 1,
                    tempoOnda = 2,
                    entradaOnda = 14
                  }
              ]
          }
      ],
    torresJogo = [],
    mapaJogo =
      [ [Terra, Terra, Relva, Relva, Agua, Agua],
        [Relva, Terra, Terra, Terra, Agua, Relva],
        [Agua, Agua, Agua, Terra, Agua, Relva],
        [Terra, Terra, Terra, Terra, Relva, Relva],
        [Terra, Relva, Relva, Relva, Relva, Relva],
        [Terra, Terra, Terra, Terra, Terra, Relva]
      ],
    inimigosJogo = [],
    lojaJogo =
      [ (30, torreFogo ),
        (20, torreGelo ),
        (20, torreResina )
      ]
  }



-- | Definição do jogo presente no nível 5
jogo5 :: Jogo
jogo5 = Jogo
  { baseJogo = Base { vidaBase = 200, posicaoBase = (4.5, 5.5), creditosBase = 80 },
    portaisJogo =
      [ Portal
          { posicaoPortal = (5.5, 0.5),
            ondasPortal =
              [ Onda 
                  { inimigosOnda =
                      [ Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] },
                        Inimigo { posicaoInimigo = (5.5, 0.5), direcaoInimigo = Oeste, vidaInimigo = 100, velocidadeInimigo = 0.5, ataqueInimigo = 200, butimInimigo = 5, projeteisInimigo = [] }
                      ],
                    cicloOnda = 1,
                    tempoOnda = 2,
                    entradaOnda = 0
                  }
              ]
          }
      ],
    torresJogo = [],
    mapaJogo =
      [ [Relva, Terra, Terra, Terra, Terra, Terra],
        [Relva, Terra, Relva, Relva, Agua, Agua],
        [Agua, Terra, Agua, Agua, Agua, Relva],
        [Terra, Terra, Relva, Relva, Relva, Relva],
        [Terra, Relva, Relva, Relva, Relva, Relva],
        [Terra, Terra, Terra, Terra, Terra, Relva]
      ],
    inimigosJogo = [],
    lojaJogo =
      [ (30, torreFogo ),
        (20, torreGelo ),
        (20, torreResina )
      ]
  }



-- | Definição de um mapa base necessário para fazer certas conversões
mapaBase :: Mapa
mapaBase =
  let r = Relva
      t = Terra
      a = Agua
  in
    [[t, t, r, r, a, a],
     [r, t, t, t, a, r],
     [a, a, a, t, a, r],
     [t, t, t, t, r, r],
     [t, r, r, r, r, r],
     [t, t, t, t, t, r]]