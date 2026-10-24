{-|
Module      : Desenhar
Description : Responsável pela renderização gráfica do jogo "Immutable Towers"
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>


O módulo `Desenhar` contém a função principal `desenha`, que utiliza a biblioteca Gloss para apresentar graficamente os diferentes elementos do jogo, incluindo o mapa, as torres, os inimigos, os portais e a base. Este módulo é responsável por transformar o estado interno do jogo numa interface visual interativa.
-}

module Desenhar where

import Graphics.Gloss
import ImmutableTowers
    ( mapaBase,
      GameState(JogoAtivo, Menu, Final, Fq, EscolherNivel),
      ImmutableTowers(menuState, gameState),
      MenuState(..) )
import LI12425
import Numeric


{-| A função `desenha` é responsável pela renderização conjunta de todos os elementos gráficos do jogo, nomeadamente:
* Menu Inical;
* Menu de escolher os níveis;
* Mapa;
* Base;
* Portais;
* Loja;
* Creditos;
* Vida da base;
* Torres;
* Inimigos;
* Imagem a dizer que ganhou ou perdeu o jogo;
* Menu Faq;
-}
desenha :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> ImmutableTowers -> Picture
desenha imgNiveis imagemMenu imagemFaq imagemTorreBase imagemTorrePortal imgFogo imgGelo imgResina imgMoedas imgCoracaoVidas imgLoja imgGanhar imgPerder imgSoldado imgGeloProj imgFogoProj imgResinaProj game =
  case gameState game of
    Menu -> desenhaMenu imagemMenu 
    EscolherNivel -> desenhaNiveis imgNiveis
    JogoAtivo jogo -> Pictures [
        desenhaMapa (mapaJogo jogo),
        desenhaBase imagemTorreBase (baseJogo jogo),
        desenhaPortais imagemTorrePortal (portaisJogo jogo),
        desenhaLoja imgLoja,
        desenhaCreditos imgMoedas jogo,
        desenhaVida imgCoracaoVidas jogo,
        desenhaTorres imgFogo imgGelo imgResina (torresJogo jogo),
        desenhaInimigos (inimigosJogo jogo) imgSoldado imgGeloProj imgFogoProj imgResinaProj
      ]
    Final "Ganhou" -> Translate 0 0 imgGanhar
    Final "Perdeu" -> Translate 0 0 imgPerder
    Fq -> Translate 0 0 imagemFaq
    _ -> Blank  -- Trata de qualquer outro estado desconhecido


{-| A função `desenhaMenu` coloca no ecrã inicial uma imagem importada no arquivo `main.hs`-}
desenhaMenu :: Picture -> Picture
desenhaMenu imagemMenu = Translate 0 0 imagemMenu


{-| A função `desenhaNiveis` coloca no ecrã secundário onde se encontram os niveis uma imagem importada no arquivo `main.hs`-}
desenhaNiveis :: Picture -> Picture
desenhaNiveis imgNiveis= Translate 0 0 imgNiveis


{-| A função `desenhaMapa é responsável por desenhar os quadrados do mapa, fazendo a conversão da notação matricial para que se possa visualizar Terra, Água e Relva -}
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = Pictures $ concatMap desenhaLinha (zip [0..] mapa)
  where
    larguraJanela = 1920
    alturaJanela = 1080

    numLinhas = length mapa
    numColunas = length (head mapa)

    tamanhoCelula = 0.9 * (min (fromIntegral larguraJanela / fromIntegral numColunas) 
                               (fromIntegral alturaJanela / fromIntegral numLinhas))

    offsetX = (fromIntegral larguraJanela - (fromIntegral numColunas * tamanhoCelula)) / 2
    offsetY = (fromIntegral alturaJanela - (fromIntegral numLinhas * tamanhoCelula)) / 2

    desenhaLinha (linha, terrenos) = map (desenhaTerreno linha) (zip [0..] terrenos)

    desenhaTerreno linha (coluna, terreno) =
      let x = (fromIntegral coluna * tamanhoCelula) - (fromIntegral larguraJanela / 2) + (tamanhoCelula / 2) + offsetX
          y = -(fromIntegral linha * tamanhoCelula) + (fromIntegral alturaJanela / 2) - (tamanhoCelula / 2) - offsetY
          cor = corTerreno terreno
      in Translate x y $ Color cor $ rectangleSolid tamanhoCelula tamanhoCelula

    corTerreno Relva = green
    corTerreno Terra = makeColor 0.545 0.271 0.075 1
    corTerreno Agua = blue


{-| A função `desenhaBase` coloca no ecrã do jogo uma imagem importada no arquivo `main.hs` fazendo a conversão da notação matricial para a janela do computador -}
desenhaBase :: Picture -> Base -> Picture
desenhaBase imagem base =
  let (x, y) = posicaoBase base
      (posX, posY) = centroCelula x y
      escala = tamanhoCelula / 500
  in Translate posX (posY +40) $ Scale escala escala imagem


{-| A função `desenhaPortais` faz map da função `desenhaPortal` em todos os portais presentes no jogo-}
desenhaPortais :: Picture -> [Portal] -> Picture
desenhaPortais imagem portais = Pictures (map (desenhaPortal imagem) portais)

{-| A função `desenhaPortal` coloca no ecrã do jogo uma imagem de um portal importada no arquivo `main.hs` fazendo a conversão da notação matricial para a janela do computador-}
desenhaPortal :: Picture -> Portal -> Picture
desenhaPortal imagem portal =
  let (x, y) = posicaoPortal portal
      (posX, posY) = centroCelula x y
  in Translate posX posY $ Scale fator fator imagem
  where
    fator = tamanhoCelula / 500


{-| A função `desenhaLoja` coloca no ecrã do jogo uma imagem de uma loja importada no arquivo `main.hs`-}
desenhaLoja :: Picture -> Picture
desenhaLoja imagemLoja =
  Translate 600 0 $ Scale 0.80 0.80 imagemLoja


{-| A função `desenhaLoja` coloca no ecrã do jogo uma imagem de moedas importada no arquivo `main.hs`-}
desenhaCreditos :: Picture -> Jogo -> Picture
desenhaCreditos imagemMoedas jogo =
  Pictures [
    Translate 550 408 $ Scale 0.2 0.2 $ Color white $ Text (show (creditosBase (baseJogo jogo))),
    Translate 520 420 $ Scale 0.1 0.1 imagemMoedas -- Redimensiona e posiciona a imagem
  ]

{-| A função `desenhaLoja` coloca no ecrã do jogo uma imagem de um coração importada no arquivo `main.hs`-}
desenhaVida :: Picture -> Jogo -> Picture
desenhaVida imagemCoracaoVidas jogo =
  Pictures [
    Translate 550 360 $ Scale 0.2 0.2 $ Color white $ Text (show (vidaBase (baseJogo jogo))),
    Translate 520 370 $ Scale 0.1 0.1 imagemCoracaoVidas -- Redimensiona e posiciona a imagem
  ]

{-| A função `desenhaTorres` faz map da função `desenhaTorre` em todos as Torres presentes no jogo-}
desenhaTorres :: Picture -> Picture -> Picture -> [Torre] -> Picture
desenhaTorres imgFogo imgGelo imgResina torres = Pictures (map (desenhaTorre imgFogo imgGelo imgResina) torres)


{-| A função `desenhaTorre` coloca no ecrã do jogo uma imagem da respetiva torre (Gelo,Fogo ou Resina) importada no arquivo `main.hs` fazendo a conversão da notação matricial para a janela do computador-}
desenhaTorre :: Picture -> Picture -> Picture -> Torre -> Picture
desenhaTorre imgFogo imgGelo imgResina torre =
  let (x, y) = posicaoTorre torre
      (posX, posY) = centroCelula x y
      imagem = case tipoProjetil (projetilTorre torre) of
        Fogo   -> imgFogo
        Gelo   -> imgGelo
        Resina -> imgResina

      tempoRestante = tempoTorre torre
      textoTempo = Translate (posX -15) (posY - 70) $
                   Scale 0.1 0.1 $
                   Color black $
                   Text (desenhaTempo tempoRestante)
  in Pictures [
        Translate posX (posY + 20) $ Scale fator fator imagem,
        textoTempo
      ]
  where
    fator = tamanhoCelula / 600 

{-| A função auxiliar `desenhaTempo` utilizada em `desenhaTorre` indica o tempo que demora até a torre ficar pronta para disparar-}
desenhaTempo :: Float -> String
desenhaTempo tempo
  | tempo <= 0 = "Pronto!"
  | otherwise  = showFFloat (Just 1) tempo "s"


{-| A função `desenhaInimigos` faz map da função `desenhaInimigos` em todos os inimigos presentes no jogo-}
desenhaInimigos :: [Inimigo] -> Picture -> Picture -> Picture -> Picture -> Picture
desenhaInimigos inimigos imgSoldado imgGeloProj imgFogoProj imgResinaProj =
  Pictures (map (desenhaInimigo imgSoldado imgGeloProj imgFogoProj imgResinaProj) inimigos)


{-| A função `desenhaInimigos` coloca no ecrã do jogo uma imagem de um inimigo importada no arquivo `main.hs` e os respetiveis projeteis pela qual o inimigo está a ser afetado fazendo a conversão da notação matricial para a janela do computador-}
desenhaInimigo :: Picture -> Picture -> Picture -> Picture -> Inimigo -> Picture
desenhaInimigo imgSoldado imgGeloProj imgFogoProj imgResinaProj inimigo =
  let (x, y) = posicaoInimigo inimigo
      (posX, posY) = centroCelula x y
      vida = vidaInimigo inimigo
      barraVida = desenhaBarraVida vida posX posY
      projeteis = projeteisInimigo inimigo
  in Pictures [
        Translate posX posY $ Scale 0.2 0.2 imgSoldado,
        barraVida,
        if Gelo `elem` map tipoProjetil projeteis then Translate (posX +10) (posY -30) $ Scale 0.1 0.1 imgGeloProj
         else Blank,
        if Fogo `elem` map tipoProjetil projeteis then Translate (posX - 15) (posY - 30) $ Scale 0.1 0.1 imgFogoProj
         else Blank,
        if Resina `elem` map tipoProjetil projeteis then Translate (posX + 35) (posY - 30) $ Scale 0.1 0.1 imgResinaProj
         else Blank
      ]


{-| A função auxiliar `desenhaBarraTempo` utilizada em `desenhaInimigo` indica a vida dos inimigos através de uma barra à medida que este vai perdendo vida-}
desenhaBarraVida :: Float -> Float -> Float -> Picture
desenhaBarraVida vida posX posY =
  let larguraBarra = tamanhoCelula * 0.4
      alturaBarra = tamanhoCelula * 0.05
      corBarra = if vida > 50 then green else if vida > 20 then yellow else red
      larguraVida = larguraBarra * (vida / 100)
  in Translate posX (posY + tamanhoCelula * 0.25) $
       Pictures [
         Color (greyN 0.2) $ rectangleSolid larguraBarra alturaBarra, 
         Color corBarra $ Translate (-larguraBarra / 2 + larguraVida / 2) 0 $ rectangleSolid larguraVida alturaBarra 
       ]


{-| A função auxiliar `centroCelula` é utilizada em diversas funções com o objetivo de contralizar objetos no meio das células-}
centroCelula x y =
  let posX = (x - fromIntegral numColunas / 2) * tamanhoCelula
      posY = -(y - fromIntegral numLinhas / 2) * tamanhoCelula
  in (posX, posY)


{-| A função auxiliar `larguraJanela` é utilizada em diversas funções para ajudar na renderização 1920x1080-}
larguraJanela = 1920

{-| A função auxiliar `alturaJanela` é utilizada em diversas funções para ajudar na renderização 1920x1080-}
alturaJanela = 1080

{-| A função auxiliar `numLinhas` é utilizada em diversas funções sabendo através dela o número de linhas presentes na matriz do mapa-}
numLinhas = length mapaBase

{-| A função auxiliar `numColunas` é utilizada em diversas funções sabendo através dela o número de colunas presentes na matriz do mapa-}
numColunas = length (head mapaBase)


{-| A função auxiliar `tamanhoCelula` é utilizada em diversas funções fazendo com que as células nunca saiam do mapa-}
tamanhoCelula = 0.9 * (min (fromIntegral larguraJanela / fromIntegral numColunas) 
                           (fromIntegral alturaJanela / fromIntegral numLinhas))

{-| A função auxiliar `offsetX` aplica um ajuste adicional para compensar qualquer margem. -}
offsetX = (fromIntegral larguraJanela - (fromIntegral numColunas * tamanhoCelula)) / 2

{-| A função auxiliar `offsetY` aplica um ajuste adicional para compensar qualquer margem. -}
offsetY = (fromIntegral alturaJanela - (fromIntegral numLinhas * tamanhoCelula)) / 2