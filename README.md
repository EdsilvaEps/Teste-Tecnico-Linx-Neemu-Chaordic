# Teste-Tecnico-Linx-Neemu-Chaordic-
Teste técnico  Linx + Neemu + Chaordic  em Scala

Para executar
1. usando o terminal, cd dentro da pasta assingment
2. sbt 'run SparkTestSetup' (ou sbt run)

o script vai ler todos os arquivos .json localizados
na pasta /tmp/sample e gerar arquivos .json processados
na pasta /tmp/new_data com os mesmos nomes
além de um arquivo 'histogram.txt' contendo os dados 
de idade (age, count) dos clientes em ordem crescente crescente de idade.


Obs: o link de tutorial no repositorio https://github.com/chaordic/ignition-template
está indisponível, só descobri onde o artigo estava depois de dois dias :D e mesmo assim,
a paste 'core' não pode ser baixada, impossibilitando a execução do código.

Obs2: Ainda não tenho uma noção boa de Spark (é a primeira vez que uso tanto spark quanto
scala), encontrei vários problemas ao tentar configurar o ambiente, por isso todo o código foi feito usando scala apenas.


