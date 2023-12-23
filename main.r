# Supondo que você tenha um dataframe chamado alunos_banco
# Vamos substituir NA por 0
alunos_banco[is.na(alunos_banco)] <- 0

# Converter a variável sexo para um fator com dois níveis
alunos_banco$sexo <- as.factor(alunos_banco$sexo)

# Criar uma tabela de contingência
tabela_contingencia <- table(alunos_banco$depressao, alunos_banco$sexo)

# Realizar o teste qui-quadrado
resultado_chi_square <- chisq.test(tabela_contingencia)

# Calcular a média, desvio padrão e valor n para homens e mulheres
media_homem <- mean(alunos_banco$depressao[alunos_banco$sexo == 1])
desvio_homem <- sd(alunos_banco$depressao[alunos_banco$sexo == 1])
n_homem <- sum(alunos_banco$sexo == 1)

media_mulher <- mean(alunos_banco$depressao[alunos_banco$sexo == 2])
desvio_mulher <- sd(alunos_banco$depressao[alunos_banco$sexo == 2])
n_mulher <- sum(alunos_banco$sexo == 2)

# Calcular a porcentagem de depressão para homens e mulheres
porcentagem_homem <- mean(alunos_banco$depressao[alunos_banco$sexo == 1]) * 100
porcentagem_mulher <- mean(alunos_banco$depressao[alunos_banco$sexo == 2]) * 100

# Imprimir os resultados do teste qui-quadrado
cat("=== Teste Qui-Quadrado ===\n")
print(resultado_chi_square)

# Imprimir os resultados para homens
cat("=== Homens ===\n")
cat("Média: ", media_homem, "\n")
cat("Desvio Padrão: ", desvio_homem, "\n")
cat("Número de Observações (n): ", n_homem, "\n")
cat("Porcentagem de Depressão: ", porcentagem_homem, "%\n\n")

# Imprimir os resultados para mulheres
cat("=== Mulheres ===\n")
cat("Média: ", media_mulher, "\n")
cat("Desvio Padrão: ", desvio_mulher, "\n")
cat("Número de Observações (n): ", n_mulher, "\n")
cat("Porcentagem de Depressão: ", porcentagem_mulher, "%\n\n")

