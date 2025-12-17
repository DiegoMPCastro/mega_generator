
# 🎱 Gerador de Sequências Mega-Sena (Shiny App)

Uma aplicação web interativa desenvolvida em **R** com o framework
**Shiny** que utiliza métodos estatísticos avançados (Inferência
Bayesiana, Análise de Pares e Atrasos) para gerar sugestões de jogos
para a loteria Mega-Sena com base em dados históricos.

## 📊 Metodologias de Análise

O algoritmo de geração não é puramente aleatório. Ele combina quatro
camadas de análise para sugerir sequências “organicamente” conectadas:

### 1. Inferência Bayesiana

Utiliza o teorema de Bayes para atualizar a probabilidade de um número
ser sorteado.

- **Priori (Prior):** A frequência histórica global de cada número
  (quantas vezes ele saiu em toda a história).

- **Verossimilhança (Likelihood):** A taxa de coocorrência (quantas
  vezes o número \$X\$ sai quando o número \$Y\$ já está no jogo).

- **Resultado:** A probabilidade de um número ser escolhido aumenta se
  ele tiver alta afinidade com os números já presentes no jogo atual.

### 2. Análise de Coocorrência (Pares)

O sistema calcula uma matriz de afinidade entre todas as 60 dezenas. Ao
gerar um jogo, se o número `10` é selecionado, o algoritmo busca quais
números historicamente mais aparecem junto com o `10`, aumentando o peso
deles na seleção subsequente.

### 3. Filtros Heurísticos (Padrões de Sorteio)

Para aumentar a previsibilidade estatística, o gerador tenta produzir
jogos que se encaixam na “Curva de Sino” dos sorteios reais:

- **Soma das Dezenas:** Busca jogos cuja soma total esteja entre **140 e
  240** (faixa onde ocorre a vasta maioria dos resultados).

- **Equilíbrio Par/Ímpar:** Evita jogos extremos (ex: 6 pares ou 6
  ímpares). Favorece distribuições como 3/3 ou 4/2.

### 4. Ponderação Temporal e Atrasos

- **Tendência Recente:** O algoritmo dá um peso (configurável no código)
  para os últimos 50 sorteios, detectando números “quentes”.

- **Análise de Atrasos:** Identifica dezenas “frias” (que não saem há
  muito tempo), permitindo visualizar oportunidades de reversão à média.

## 🚀 Como Executar

### Pré-requisitos

Você precisará do **R** e do **RStudio** instalados. Além disso, instale
os pacotes necessários executando o comando abaixo no console do R:

    install.packages(c("shiny", "readxl", "dplyr", "ggplot2", "DT", "bslib"))

### Rodando a Aplicação

1.  Clone este repositório ou baixe o arquivo `app.R`.

2.  Abra o `app.R` no RStudio.

3.  Clique no botão **“Run App”** (canto superior direito do editor de
    script).

4.  No navegador que abrir, faça o upload do arquivo Excel com o
    histórico de sorteios.

### Formato dos Dados

O app espera um arquivo `.xlsx` (Excel) contendo o histórico dos
sorteios.

- O sistema tenta detectar automaticamente as colunas das dezenas
  (procurando por “Bola”, “Dezena”, etc).

- Recomenda-se baixar a planilha oficial ou do site *As Loterias*.

## 🛠️ Funcionalidades da Interface

- **Upload de Arquivo:** Carregamento simples de base de dados `.xlsx`.

- **Número Pivô (Opcional):**

  - *Com Pivô:* Você escolhe um número fixo (ex: seu número da sorte) e
    o sistema gera o resto do jogo baseado na afinidade estatística com
    ele.

  - *Sem Pivô:* O sistema escolhe o primeiro número baseado na
    estatística global e constrói o resto do jogo organicamente.

- **Gráficos Interativos:**

  - Frequência dos números.

  - Mapa de calor de coocorrência (quais números “se atraem”).

  - Gráfico de Atrasos (dezenas que estão “dormindo”).

- **Tabela de Resultados:** Exibe os jogos gerados com indicadores de
  Soma e Paridade.

## ⚠️ Disclaimer (Aviso Legal)

Este software foi desenvolvido para fins **educacionais e de
entretenimento**, demonstrando a aplicação de estatística e programação
R em dados reais.

> **A loteria é um jogo de azar.** Nenhum método estatístico garante
> vitória. Todos os sorteios são eventos independentes e as
> probabilidades matemáticas de qualquer combinação específica (ex:
> `01-02-03-04-05-06` vs `10-23-34-45-51-59`) são idênticas. Jogue com
> responsabilidade.

## 📄 Licença

Este projeto está sob a licença MIT. Sinta-se livre para modificar e
distribuir.
