{-|
Module      : Main
Description : Ponto de entrada do jogo "Immutable Towers"
Copyright   : Francisco da Cunha Carvalho <a110551@alunos.uminho.pt>
              Ricardo Filipe Lopes Soares <a110585@alunos.uminho.pt>

O módulo `Main` define a função principal do jogo "Immutable Towers", utilizando a biblioteca Gloss para inicializar a janela gráfica, carregar as imagens necessárias e iniciar o loop principal do jogo, com as funções de desenho e resposta a eventos.
-}

module Main where

import Desenhar        -- Módulo responsável por desenhar os elementos gráficos do jogo
import Eventos         -- Módulo que lida com os eventos de entrada (rato, teclado)
import Graphics.Gloss  -- Biblioteca para criação da interface gráfica
import ImmutableTowers -- Definição do tipo principal `ImmutableTowers`
import Tempo           -- Módulo que atualiza o estado do jogo com base no tempo decorrido

-- | Definição das configurações da janela gráfica.
janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)
-- A janela do jogo é criada com o título "Immutable Towers", dimensões de 1920x1080 pixels
-- e é posicionada no canto superior esquerdo do ecrã.

-- | Cor de fundo da janela gráfica.
fundo :: Color
fundo = azure
-- Define a cor de fundo da janela como azul-claro.

-- | Definição da taxa de frames por segundo (FPS).
fr :: Int
fr = 60
-- A atualização da janela gráfica ocorre a uma taxa de 60 frames por segundo.

-- | Função que carrega todas as imagens necessárias para o jogo.
carregaImagens :: IO (Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture)
carregaImagens = do
  -- Cada imagem é carregada a partir de um ficheiro BMP localizado na pasta "images".
  geloProj <- loadBMP "images/geloProjetil.bmp"
  fogoProj <- loadBMP "images/fogoProjetil.bmp"
  resinaProj <- loadBMP "images/resinaProjetil.bmp"
  soldado  <- loadBMP "images/soldadoJogo.bmp"
  ganhar <- loadBMP "images/ganharJogo.bmp"
  perder <- loadBMP "images/perderJogo.bmp"
  loja <- loadBMP "images/menuLoja.bmp"
  moedas <- loadBMP "images/moedasJogo.bmp"
  coracao <- loadBMP "images/coracaoVidas.bmp"
  fogo <- loadBMP "images/TorreFogo.bmp"
  gelo <- loadBMP "images/TorreGelo.bmp"
  resina <- loadBMP "images/TorreResina.bmp"
  base <- loadBMP "images/TorreBase.bmp"
  portal <- loadBMP "images/TorrePortal.bmp"
  faq <- loadBMP "images/imagemFaq.bmp"
  menu <- loadBMP "images/ImagemMenu.bmp"
  niveis <- loadBMP "images/ImagemNiveis.bmp"
  -- As imagens são devolvidas como um conjunto de 20 elementos.
  return (niveis, menu, faq, base, portal, fogo, gelo, resina, moedas, coracao, loja, ganhar, perder, soldado, geloProj, fogoProj, resinaProj)

-- | Função principal do jogo.
main :: IO ()
main = do
  -- As imagens carregadas são extraídas para variáveis específicas.
  (imagemNiveis, imagemMenu, imagemFaq, imagemTorreBase, imagemTorrePortal, imgFogo, imgGelo, imgResina, imgMoedas, imgCoracaoVidas, imgLoja, imgGanhar, imgPerder, imgSoldado, imgGeloProj, imgFogoProj, imgResinaProj) <- carregaImagens
  
  -- Inicialização do loop principal do jogo com a função `play` da biblioteca Gloss.
  play janela fundo fr estadoInicial 
    -- Função responsável por desenhar o jogo ( no arquivo `Desenhar.hs` ).
    (desenha imagemNiveis imagemMenu imagemFaq imagemTorreBase imagemTorrePortal imgFogo imgGelo imgResina imgMoedas imgCoracaoVidas imgLoja imgGanhar imgPerder imgSoldado imgGeloProj imgFogoProj imgResinaProj)  
    -- Função que reage a eventos do utilizador (rato e teclado). ( no arquivo `Eventos.hs`)
    reageEventos 
    -- Função que atualiza o estado do jogo com base no tempo decorrido. ( no arquivo `Tempo.hs`)
    reageTempo 
  where
    -- Estado inicial do jogo.
    estadoInicial = ImmutableTowers
      { menuState = Start,        -- O jogo inicia no estado de menu inicial.
        gameState = Menu          -- O estado inicial do jogo é o Menu.
      }