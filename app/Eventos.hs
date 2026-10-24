{-|
Module      : Eventos
Description : Gestão dos eventos de entrada do jogo "Immutable Towers"
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>

O módulo `Eventos` contém as funções que tratam os eventos de entrada, como cliques do rato e movimentos, alterando o estado do jogo de acordo com a interação do utilizador. Estes eventos incluem navegar pelos menus, selecionar níveis e interagir com o mapa do jogo.
-}

module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425

-- | Função que reage a eventos do utilizador e atualiza o estado do jogo.
reageEventos :: Event           -- ^ Evento de entrada (como um clique do rato).
             -> ImmutableTowers -- ^ Estado atual do jogo.
             -> ImmutableTowers -- ^ Novo estado do jogo após a interação.

-- | Trata eventos de clique do botão esquerdo do rato.
reageEventos (EventKey (MouseButton LeftButton) Up _ (x, y)) game =
    case gameState game of
        -- Evento quando o jogo está no Menu.
        Menu ->
            if x > -220 && x < 220 && y > -100 && y < 70 then
                game { gameState = EscolherNivel }
            else if x > -165 && x < 165 && y > -300 && y < -125 then
                game { gameState = Fq }
            else
                game

        -- Evento na fase de escolher nível.
        EscolherNivel ->
            if x > -130 && x < 130 && y > 195 && y < 300 then
                game { gameState = JogoAtivo jogo1 }
            else if x > -130 && x < 130 && y > 70 && y < 175 then
                game { gameState = JogoAtivo jogo2 }
            else if x > -130 && x < 130 && y > -50 && y < 50 then
                game { gameState = JogoAtivo jogo3 }
            else if x > -130 && x < 130 && y > -175 && y < -75 then
                game { gameState = JogoAtivo jogo4 }
            else if x > -130 && x < 130 && y > -300 && y < -195 then
                game { gameState = JogoAtivo jogo5 }
            else
                game

        -- Evento quando o jogo está ativo.
        JogoAtivo jogo ->
            if x > 520 && x < 680 && y > 110 && y < 260 then
                game { torreSelecionada = Just "Fogo" }
            else if x > 520 && x < 680 && y > -80 && y < 80 then
                game { torreSelecionada = Just "Gelo" }
            else if x > 520 && x < 680 && y > -260 && y < -110 then
                game { torreSelecionada = Just "Resina" }
            else colocaTorreSelecionada (x, y) game

        -- Evento quando o jogo está no menu de FAQ.
        Fq ->
            if x > -600 && x < 600 && y > -800 && y < 800 then
                game { gameState = Menu }
            else
                game

        -- Se o estado não for tratado, retorna o jogo inalterado.
        _ -> game

-- | Reage ao pressionar da tecla Enter.
reageEventos (EventKey (SpecialKey KeyEnter) Up _ _) game =
    case gameState game of
        -- Quando o jogo está no estado Final (vitória ou derrota), volta para o Menu.
        Final _ -> game { gameState = Menu, menuState = Start }

        -- Quando o jogo está no menu de FAQ, volta para o Menu.
        Fq -> game { gameState = Menu }

        -- Outros estados não reagem ao Enter.
        _ -> game

-- | Ignora outros eventos que não sejam clique do rato.
reageEventos _ game = game

-- | Função que tenta colocar uma torre no mapa, dependendo da posição do clique e do tipo de torre selecionado.
colocaTorreSelecionada :: (Float, Float) -> ImmutableTowers -> ImmutableTowers
colocaTorreSelecionada (x, y) game =
    case (torreSelecionada game, gameState game) of
        (Just tipo, JogoAtivo jogo) ->
            let (linha, coluna) = posicaoMapa x y (mapaJogo jogo)
                terreno = (mapaJogo jogo) !! linha !! coluna
                preco = precoTorre tipo
            in if terreno == Relva && creditosBase (baseJogo jogo) >= preco
               then game
                 { gameState = JogoAtivo (adicionaTorre linha coluna tipo jogo preco),
                   torreSelecionada = Nothing
                 }
               else game
        _ -> game

-- | Função que converte as coordenadas do clique no ecrã para as coordenadas da célula correspondente no mapa do jogo.
posicaoMapa :: Float   -- ^ Posição X do clique.
            -> Float   -- ^ Posição Y do clique.
            -> Mapa    -- ^ Mapa atual do jogo.
            -> (Int, Int) -- ^ Coordenadas da célula no mapa.

posicaoMapa x y mapa =
    let larguraJanela = 1920
        alturaJanela = 1080

        numLinhas = length mapa
        numColunas = length (head mapa)

        tamanhoCelula = 0.9 * (min (fromIntegral larguraJanela / fromIntegral numColunas)
                                   (fromIntegral alturaJanela / fromIntegral numLinhas))

        offsetX = (fromIntegral larguraJanela - (fromIntegral numColunas * tamanhoCelula)) / 2
        offsetY = (fromIntegral alturaJanela - (fromIntegral numLinhas * tamanhoCelula)) / 2

        coluna = floor ((x + (fromIntegral larguraJanela / 2) - offsetX) / tamanhoCelula)
        linha = floor (((fromIntegral alturaJanela / 2) - y - offsetY) / tamanhoCelula)
    in (linha, coluna)

-- | Função que adiciona uma nova torre ao jogo.
adicionaTorre :: Int    -- ^ Linha onde a torre será colocada.
              -> Int    -- ^ Coluna onde a torre será colocada.
              -> String -- ^ Tipo de torre ("Fogo", "Gelo" ou "Resina").
              -> Jogo   -- ^ Estado atual do jogo.
              -> Int    -- ^ Preço da torre.
              -> Jogo   -- ^ Novo estado do jogo com a torre adicionada.

adicionaTorre linha coluna tipo jogo preco =
    let novaTorre = case tipo of
          "Fogo" -> torreFogo { posicaoTorre = (fromIntegral coluna + 0.5, fromIntegral linha + 0.5) }
          "Gelo" -> torreGelo { posicaoTorre = (fromIntegral coluna + 0.5, fromIntegral linha + 0.5) }
          "Resina" -> torreResina { posicaoTorre = (fromIntegral coluna + 0.5, fromIntegral linha + 0.5) }
          _ -> error "Tipo de torre inválido"
    in jogo
        { torresJogo = novaTorre : torresJogo jogo,
          baseJogo = (baseJogo jogo) { creditosBase = creditosBase (baseJogo jogo) - preco }
        }

-- | Função que devolve o preço de cada tipo de torre.
precoTorre :: String -- ^ Tipo de torre.
           -> Int    -- ^ Preço da torre.

precoTorre "Fogo" = 30
precoTorre "Gelo" = 20
precoTorre "Resina" = 20
precoTorre _ = error "Tipo de torre inválido"