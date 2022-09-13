            # Trabalho de Computação em Estatística 2 - UnB - 2022.1
                      # Professor Guilherme Souza
      # Alunos: Letícia Lino, Lucas Augusto, Lucas Franca e Pedro Aguiar
                        # Tema: Análise de Texto


# Aplicações do Capítulo 1 - Text Mining with R ----

## Lendo os artigos PDF e fazendo alterações

require(pdftools)
require(stringr)
require(dplyr)
getwd()
setwd("~/UNB/2022-1/CE2 - R/Seminário")

humorepiadas <- pdf_text("A piada na era da mídia social - alt.pdf") %>% 
  str_c(collapse = "") %>% 
  str_replace_all(pattern = "\n", replacement = " PAGINA ") %>% 
  str_squish() %>% 
  str_split(pattern = "PAGINA") %>% 
  unlist

revsociais <- pdf_text("Revoluções Sociais - alt.pdf") %>% 
  str_c(collapse = "") %>% 
  str_replace_all(pattern = "\n", replacement = " PAGINA ") %>% 
  str_squish() %>% 
  str_split(pattern = "PAGINA") %>% 
  unlist

tdahmulheres <- pdf_text("Os esforços, lutas e sucessos de mulheres adultas com TDAH - alt.pdf")%>% 
  str_c(collapse = "") %>% 
  str_replace_all(pattern = "\n", replacement = " PAGINA ") %>% 
  str_squish() %>% 
  str_split(pattern = "PAGINA") %>% 
  unlist


## Transforma em um data frame de palavras

require(tidytext)

humorepiada_df <- humorepiadas %>% 
  as.data.frame() %>% 
  mutate(numerolinha = row_number()) %>% 
  rename("texto" = ".") %>% 
  unnest_tokens(word, texto) %>% 
  as_tibble() %>% 
  mutate(Autor = "Sturges")

revsociais_df <- revsociais %>%
  as.data.frame() %>% 
  mutate(numerolinha = row_number()) %>% 
  rename("texto" = ".") %>% 
  unnest_tokens(word, texto) %>% 
  as_tibble() %>% 
  mutate(Autor = "Tiruneh")

tdahmulheres_df <- tdahmulheres %>% 
  as.data.frame() %>% 
  mutate(numerolinha = row_number()) %>% 
  rename("texto" = ".") %>% 
  unnest_tokens(word, texto) %>% 
  as_tibble() %>% 
  mutate(Autor = "Glaser and Langvik")


## Removendo palavras "vazias"

data(stop_words)
humorepiada_df <- humorepiada_df %>% 
  anti_join(stop_words)

data(stop_words)
revsociais_df <- revsociais_df %>% 
  anti_join(stop_words)

data(stop_words)
tdahmulheres_df <- tdahmulheres_df %>% 
  anti_join(stop_words)


## Contagem

humorepiada_df %>% count(word, sort = TRUE) #joke e jokes estão divergindo
revsociais_df %>% count(word, sort = TRUE) #revolution e revolutions estão divergindo
tdahmulheres_df %>% count(word, sort = TRUE) 


## Visualização
 
require(ggplot2)

humorepiada_df %>% 
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

revsociais_df %>% 
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

tdahmulheres_df %>% 
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


## Comparação dos artigos

require(tidyr)

frequencia <- bind_rows(mutate(humorepiada_df, Autor = "Sturges"),
                        mutate(revsociais_df, Autor = "Tiruneh"), 
                        mutate(tdahmulheres_df, Autor = "Glaser and Langvik")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(Autor, word) %>%
  group_by(Autor) %>%
  mutate(Proporção = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Autor, values_from = Proporção) 

#%>%
  #pivot_longer(`Sturges`:`Tiruneh`,
               #names_to = "Autor", values_to = "Proporção")
frequencia

## Visualizando

ggplot(frequencia, aes(x = word, y = Sturges)) +
  geom_point() +
  ylim(0, 0.03) # terminar gráfico!!!



# Aplicações do Capítulo 2 - Text Mining with R ----

## Bibliotecas para análise de sentimentos

require(textdata)

get_sentiments("afinn") # scale -5 (negative) to 5 (positive)
get_sentiments("bing") # negative or positive
get_sentiments("nrc") # categories (negative, positive, fear, trust, etc)


## Filtrando

## NRC - medo

nrc_medo <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

revsociais_df %>%
  inner_join(nrc_medo) %>%
  count(word, sort = TRUE)

## BING - positivo

bing_pos <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

humorepiada_df %>% 
  inner_join(bing_pos) %>% 
  count(word, sort = TRUE)

## AFINN - escala -3

afinn_tresneg <- get_sentiments("afinn") %>% 
  filter(value == -3)

tdahmulheres_df %>% 
  inner_join(afinn_tresneg) %>% 
  count(word, sort = TRUE)
  

## Palavras positivas e negativas - quantidade

### Humor e piada

humorepiada_sent_bing <- humorepiada_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("bing")) #jokes é negativo; não tem comedy;

humorepiada_sent_afinn <- humorepiada_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("afinn")) # jokes é 2; comedy é 1;

humorepiada_sent_nrc <- humorepiada_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("nrc")) # jokes é negativo; não tem comedy;

### Revoluções sociais

revsociais_sent_bing <- revsociais_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("bing")) #tem revolutionary mas não tem revolution(s); popular é positivo;
# defeat é positivo;

revsociais_sent_afinn <- revsociais_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("afinn")) # não tem revolution - political - social; 
#talvez algumas palavras não tem classificação?

revsociais_sent_nrc <- revsociais_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("nrc")) #revolution tem vários sentimentos # military = fear
#defeat é negativo

### TDAH mulheres

tdahmulheres_sent_bing <- tdahmulheres_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("bing")) #symptoms é negativo; lack é negativo; 
#a maioria das palavras mais frequentes não aparecem também

tdahmulheres_sent_afinn <- tdahmulheres_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("afinn")) #as 10 palavras mais frequentes não aparecem (neutralidade)?;
#positive é 2 e negative é -2; 

tdahmulheres_sent_nrc <- tdahmulheres_df %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments("nrc")) #multisentimentalidade; esteem = alegria, positivo e tristeza;


# Visualização 

## BING - Humor e piadas

humorepiadas_maisfreqs_neg <- filter(humorepiada_sent_bing, `sentiment` == "negative")

ggplot(humorepiadas_maisfreqs_neg[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "Humor & Piadas", 
       subtitle = "BING - NEGATIVE") + 
  coord_flip()

humorepiadas_maisfreqs_pos <- filter(humorepiada_sent_bing, `sentiment` == "positive")

ggplot(humorepiadas_maisfreqs_pos[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "Humor & Piadas", 
       subtitle = "BING - POSITIVE") + 
  coord_flip()

ggplot(humorepiada_sent_bing[1:20,], aes(x = reorder(word, n), n)) +
  geom_col(aes(fill = sentiment)) +
  labs(x = "Palavra", y = "Quantidade", title = "Humor & Piadas", 
       subtitle = "BING") +
  coord_flip()


## BING - Rev sociais

revsociais_maisfreqs_neg <- filter(revsociais_sent_bing, `sentiment` == "negative")

ggplot(revsociais_maisfreqs_neg[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "Revoluções Sociais", 
       subtitle = "BING - NEGATIVE") + 
  coord_flip()

revsociais_maisfreqs_pos <- filter(revsociais_sent_bing, `sentiment` == "positive")

ggplot(revsociais_maisfreqs_pos[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "Revoluções Sociais", 
       subtitle = "BING - POSITIVE") + 
  coord_flip()

ggplot(revsociais_sent_bing[1:20,], aes(x = reorder(word, n), n)) +
  geom_col(aes(fill = sentiment)) +
  labs(x = "Palavra", y = "Quantidade", title = "Revoluções Sociais", 
       subtitle = "BING") +
  coord_flip()


## BING - TDAH mulheres

tdahmulheres_maisfreqs_neg <- filter(tdahmulheres_sent_bing, `sentiment` == "negative")

ggplot(tdahmulheres_maisfreqs_neg[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "TDAH & Mulheres", 
       subtitle = "AF") + 
  coord_flip()

tdahmulheres_maisfreqs_pos <- filter(tdahmulheres_sent_bing, `sentiment` == "positive")

ggplot(tdahmulheres_maisfreqs_pos[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  facet_wrap(~sentiment) +
  labs(x = "Quantidade", y = "Palavra", title = "Humor & Piadas", 
       subtitle = "BING - POSITIVE") + 
  coord_flip()

ggplot(tdahmulheres_sent_bing[1:20,], aes(x = reorder(word, n), n)) +
  geom_col(aes(fill = sentiment)) +
  labs(x = "Palavra", y = "Quantidade", title = "Humor & Piadas", 
       subtitle = "BING") +
  coord_flip()


## AFFIN

ggplot(humorepiada_sent_afinn[1:20,], aes(x = value, y = n)) +
  geom_col() +
  labs(x = "Valor", y = "Quantidade", title = "Humor & Piadas",
       subtitle = "AFINN") +
  scale_x_continuous(breaks = seq(-5, 5, 1), limits = c(-5,5))


ggplot(revsociais_sent_afinn[1:20,], aes(x = value, y = n)) +
  geom_col() +
  labs(x = "Valor", y = "Quantidade", title = "Revoluções Sociais",
       subtitle = "AFINN") +
  scale_x_continuous(breaks = seq(-5, 5, 1), limits = c(-5,5))


ggplot(tdahmulheres_sent_afinn[1:20,], aes(x = value, y = n)) +
  geom_col() +
  labs(x = "Valor", y = "Quantidade", title = "TDAH & Mulheres",
       subtitle = "AFINN") +
  scale_x_continuous(breaks = seq(-5, 5, 1), limits = c(-5,5))


## NRC

humorepiada_confiança <- filter(humorepiada_sent_nrc, 
                                `sentiment` == "trust")

ggplot(humorepiada_confiança[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  labs(x = "Palavra", y = "Quantidade", title = "Humor & Piadas",
       subtitle = "NRC - Confiança") +
  coord_flip()

revsociais_medo <- filter(revsociais_sent_nrc,
                          `sentiment` == "fear")

ggplot(revsociais_medo[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  labs(x = "Palavra", y = "Quantidade", title = "Revoluções Sociais",
       subtitle = "NRC - Medo") +
  coord_flip()

tdahmulheres_raiva <- filter(tdahmulheres_sent_nrc,
                             `sentiment` == "anger")

ggplot(tdahmulheres_raiva[1:20,], aes(x = reorder(word, n), n)) +
  geom_col() +
  labs(x = "Palavra", y = "Quantidade", title = "TDAH & Mulheres",
       subtitle = "NRC - Raiva") + 
  coord_flip()


# Comparando bibliotecas

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


# Nuvens de palavras

require(wordcloud)
require(reshape2)

humorepiada_df %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

revsociais_df %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

tdahmulheres_df %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))


humorepiada_df %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

revsociais_df %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

tdahmulheres_df %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)



#APRESENTAÇÃO QMD***
require(tm)
require(ggraph)
require(igraph)