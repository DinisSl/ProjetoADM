# ==============================================================================
# 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES
# ==============================================================================
packages <- c("foreign", "dplyr", "tidyr", "ggplot2", "psych", "corrplot", "scales")

# Instalar pacotes que faltem
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Carregar bibliotecas
lapply(packages, require, character.only = TRUE)

# ==============================================================================
# 2. CARREGAMENTO DOS DADOS
# ==============================================================================
filename <- "C:/Users/user/Desktop/IGE/2º Ano/ADM/Projeto/ESS10_parcial_ADM_all_countries.rds"

# Verifica se o ficheiro existe antes de tentar ler
if(file.exists(filename)) {
  original <- readRDS(filename)
} else {
  stop("ERRO: Ficheiro não encontrado. Verifique o caminho em 'filename'.")
}

# Filtrar apenas Portugal
only_portugal <- original[original$cntry == "Portugal",]

# ==============================================================================
# 3. RECODIFICAÇÃO (SOCIODEMOGRÁFICA)
# ==============================================================================
tabela_recodificada <- only_portugal %>%
  mutate(
    # --- Estado Civil ---
    marsts = case_when(
      marsts %in% c("Legally married", "In a legally registered civil union") ~ "In relationship",
      marsts %in% c("Legally separated", "Legally divorced/Civil union dissolved", "Widowed/Civil partner died") ~ "Formerly in relationship",
      marsts == "None of these (NEVER married or in legally registered civil union)" ~ "Never in relationship",
      TRUE ~ NA_character_ 
    ),
    
    # --- Escolaridade ---
    edulvlb = case_when(
      edulvlb %in% c("Not completed ISCED level 1", "ISCED 1, completed primary education", "Vocational ISCED 2C < 2 years, no access ISCED 3", "General/pre-vocational ISCED 2A/2B, access ISCED 3 vocational", "General ISCED 2A, access ISCED 3A general/all 3", "Vocational ISCED 2C >= 2 years, no access ISCED 3", "Vocational ISCED 2A/2B, access ISCED 3 vocational", "Vocational ISCED 2, access ISCED 3 general/all") ~ "EduLevel:Low",
      edulvlb %in% c("Vocational ISCED 3C < 2 years, no access ISCED 5", "General ISCED 3 >=2 years, no access ISCED 5", "General ISCED 3A/3B, access ISCED 5B/lower tier 5A", "General ISCED 3A, access upper tier ISCED 5A/all 5", "Vocational ISCED 3C >= 2 years, no access ISCED 5", "Vocational ISCED 3A, access ISCED 5B/ lower tier 5A", "Vocational ISCED 3A, access upper tier ISCED 5A/all 5", "General ISCED 4A/4B, access ISCED 5B/lower tier 5A", "General ISCED 4A, access upper tier ISCED 5A/all 5", "ISCED 4 programmes without access ISCED 5", "Vocational ISCED 4A/4B, access ISCED 5B/lower tier 5A", "Vocational ISCED 4A, access upper tier ISCED 5A/all 5") ~ "EduLevel:Medium",
      edulvlb %in% c("ISCED 5A short, intermediate/academic/general tertiary below bachelor", "ISCED 5B short, advanced vocational qualifications", "ISCED 5A medium, bachelor/equivalent from lower tier tertiary", "ISCED 5A medium, bachelor/equivalent from upper/single tier tertiary", "ISCED 5A long, master/equivalent from lower tier tertiary", "ISCED 5A long, master/equivalent from upper/single tier tertiary", "ISCED 6, doctoral degree") ~ "EduLevel:High",
      TRUE ~ NA_character_
    )
  )

# Converter Variáveis para Tipos Corretos
# Fatores
tabela_recodificada$marsts <- factor(tabela_recodificada$marsts, levels = c("In relationship", "Formerly in relationship", "Never in relationship"))
tabela_recodificada$edulvlb <- factor(tabela_recodificada$edulvlb, levels = c("EduLevel:Low", "EduLevel:Medium", "EduLevel:High"))

# Numérica (Idade) - Resolve o erro "Calling var(x) on a factor"
tabela_recodificada$agea <- as.numeric(as.character(tabela_recodificada$agea))

# ==============================================================================
# 4. ANÁLISE DESCRITIVA E GRÁFICOS
# ==============================================================================

# A. Idade (Histograma e Resumo)
cat("\n>>> ESTATÍSTICAS DA IDADE <<<\n")
print(summary(tabela_recodificada$agea))
cat("Desvio Padrão:", sd(tabela_recodificada$agea, na.rm = TRUE), "\n")

g1 <- ggplot(tabela_recodificada, aes(x = agea)) +
  geom_histogram(binwidth = 5, fill = "#4E84C4", color = "white") +
  labs(title = "Distribuição de Idade", x = "Idade", y = "Frequência") + theme_minimal()
print(g1)

# B. Género (Tabela)
cat("\n>>> TABELA DE GÉNERO <<<\n")
tabela_genero <- tabela_recodificada %>%
  filter(!is.na(gndr)) %>%
  count(gndr) %>%
  mutate(percentagem = n / sum(n) * 100)
print(tabela_genero)

# C. Escolaridade (Gráfico de Barras)
g2 <- tabela_recodificada %>%
  filter(!is.na(edulvlb)) %>%
  ggplot(aes(x = edulvlb, fill = edulvlb)) +
  geom_bar() + scale_fill_brewer(palette = "Set2") +
  labs(title = "Nível de Escolaridade", x = NULL, y = "Contagem") +
  theme_minimal() + theme(legend.position = "none")
print(g2)

# D. Estado Civil (Gráfico Circular/Pie Chart)
dados_pizza <- tabela_recodificada %>%
  filter(!is.na(marsts)) %>%
  count(marsts) %>%
  mutate(prop = n / sum(n) * 100)

g4 <- ggplot(dados_pizza, aes(x = "", y = prop, fill = marsts)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() + 
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Estado Civil", fill = "Estado Civil") +
  scale_fill_brewer(palette = "Dark2")
print(g4)

# ==============================================================================
# 5. ANÁLISE EM COMPONENTES PRINCIPAIS (ACP) - VALORES HUMANOS
# ==============================================================================

# Seleção das Variáveis (NOMES CORRIGIDOS)
vars_schwartz <- c("BeCreative", "BeRich", "EqualOpport", "BeAdmired", 
                   "BeSecure", "TryNew", "FollowRules", "UnderstandPeople", 
                   "BeHumble", "HaveGoodTime", "BeFree", "HelpPeople", 
                   "Success", "StrongGov", "SeekAdv", "BehaveProp", 
                   "GetRespect", "BeLoyal", "CareNature", "Traditionalism", 
                   "SeekFun")

# Criar tabela apenas com estas variáveis e remover NAs
dados_acp <- tabela_recodificada %>%
  select(all_of(vars_schwartz)) %>%
  na.omit()

# Converter tudo para numérico (para garantir que a matemática funciona)
dados_acp <- as.data.frame(lapply(dados_acp, function(x) as.numeric(as.character(x))))

cat("\n>>> DADOS PREPARADOS PARA ACP <<<\n")
cat("Nº de observações válidas:", nrow(dados_acp), "\n")

# --- A. Matriz de Correlação e Testes de Adequabilidade ---
cor_matrix <- cor(dados_acp)
corrplot(cor_matrix, method = "color", tl.cex = 0.6, title = "Matriz Correlação", mar=c(0,0,1,0))

cat("\n>>> Teste KMO (Deve ser > 0.6) <<<\n")
print(KMO(dados_acp))

cat("\n>>> Teste de Bartlett (Deve ser p < 0.05) <<<\n")
print(cortest.bartlett(cor_matrix, n = nrow(dados_acp)))

# --- B. Critério de Kaiser e Scree Plot ---
pca_inicial <- principal(dados_acp, nfactors = 21, rotate = "none")
eigenvalues <- pca_inicial$values

plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Componentes", ylab = "Eigenvalues")
abline(h = 1, col = "red", lty = 2)

num_comp_reter <- sum(eigenvalues > 1)
cat("\n>>> Número de componentes a reter (Kaiser):", num_comp_reter, "\n")

# --- C. ACP Final com Rotação Varimax ---
pca_final <- principal(dados_acp, nfactors = num_comp_reter, rotate = "varimax", scores = TRUE)

cat("\n>>> Cargas Fatoriais (Loadings) - Dimensões Interpretáveis <<<\n")
print.psych(pca_final, cut = 0.4, sort = TRUE)

cat("\n>>> Variância Explicada <<<\n")
print(pca_final$Vaccounted)

# --- D. Guardar os Scores (Opcional) ---
scores_df <- as.data.frame(pca_final$scores)
colnames(scores_df) <- paste0("Dimensao_", 1:ncol(scores_df))
cat("\n>>> Primeiras linhas dos scores calculados: <<<\n")
print(head(scores_df))



# ==============================================================================
# EXERCÍCIO 3: RELAÇÃO ENTRE DIMENSÕES (ACP) E ESCOLARIDADE
# ==============================================================================

# 1. PREPARAÇÃO DOS DADOS (Garantir alinhamento das linhas)
# Selecionar as variáveis da ACP + a variável de escolaridade
vars_analise <- c(vars_schwartz, "edulvlb")

# Criar novo dataframe limpo para garantir que as linhas coincidem
dados_final <- tabela_recodificada %>%
  select(all_of(vars_analise)) %>%
  na.omit() # Remove NAs de TODAS as variáveis (incluindo escolaridade)

# Separar apenas as colunas numéricas para a ACP
dados_numericos_acp <- dados_final %>% select(-edulvlb)

# 2. RECALCULAR SCORES (Baseado na ACP feita no passo anterior)
# Nota: Usamos os loadings e estrutura definidos anteriormente
# Assumindo que 'num_comp_reter' foi definido na secção 5 do teu script
pca_ex3 <- principal(dados_numericos_acp, nfactors = num_comp_reter, rotate = "varimax", scores = TRUE)

# Adicionar os scores ao dataframe que tem a escolaridade
scores <- as.data.frame(pca_ex3$scores)
# Renomear colunas para facilitar (Dim1, Dim2, etc.)
colnames(scores) <- paste0("Dimensao_", 1:ncol(scores))
dados_com_scores <- cbind(dados_final, scores)

# 3. ANÁLISE DE VARIÂNCIA (ANOVA) E GRÁFICOS
# Vamos fazer um loop ou repetir para cada dimensão encontrada (ex: Dimensao_1, Dimensao_2...)

lista_dimensoes <- colnames(scores)

for(dim in lista_dimensoes) {
  cat(paste0("\n=======================================================\n"))
  cat(paste0(">>> ANÁLISE PARA: ", dim, " <<<\n"))
  cat(paste0("=======================================================\n"))
  
  # A. Estatísticas Descritivas por Grupo
  print(dados_com_scores %>% 
          group_by(edulvlb) %>% 
          summarise(
            Media = mean(get(dim)),
            DesvioPadrao = sd(get(dim)),
            n = n()
          ))
  
  # B. Teste ANOVA
  formula_anova <- as.formula(paste(dim, "~ edulvlb"))
  modelo_anova <- aov(formula_anova, data = dados_com_scores)
  
  cat("\n--- Tabela ANOVA ---\n")
  print(summary(modelo_anova))
  
  # C. Verificação de Pressupostos (Simplificado)
  # 1. Homogeneidade de Variâncias (Teste de Bartlett)
  # H0: Variâncias são iguais (Homocedasticidade)
  cat("\n--- Teste de Homogeneidade de Variâncias (Bartlett) ---\n")
  print(bartlett.test(formula_anova, data = dados_com_scores))
  
  # D. Testes Post-Hoc (Tukey) - Apenas se ANOVA for significativa (p < 0.05)
  valor_p_anova <- summary(modelo_anova)[[1]][["Pr(>F)"]][1]
  
  if(valor_p_anova < 0.05) {
    cat("\n>>> RESULTADO SIGNIFICATIVO: A realizar teste Post-Hoc de Tukey... <<<\n")
    print(TukeyHSD(modelo_anova))
  } else {
    cat("\n>>> Não significativo. Não há diferenças estatísticas entre os grupos.\n")
  }
  
  # E. Visualização (Boxplot)
  grafico <- ggplot(dados_com_scores, aes(x = edulvlb, y = .data[[dim]], fill = edulvlb)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
    labs(title = paste("Distribuição de", dim, "por Nível de Escolaridade"),
         subtitle = "Ponto vermelho indica a média",
         x = "Nível de Escolaridade",
         y = "Score da Dimensão (Padronizado)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    theme(legend.position = "none")
  
  print(grafico)
}
