setwd("~/GitHub/progamacion_Actuarial_III/Caso3/UCI HAR Dataset")
Juntar1 = rbind(read.table("./Caso3/train/X_train.txt"), 
                read.table("./Caso3/test/X_test.txt"))
Juntar2 = rbind(read.table("./Caso3/train/y_train.txt"),
                read.table("./Caso3/test/y_test.txt"))[,1]
Juntar3 = rbind(read.table("./Caso3/train/subject_train.txt"),
                  read.table("./Caso3/test/subject_test.txt"))
Titulo = read.table("./Caso3/features.txt")


colnames(Juntar1) <- Titulo[,2]
Mediastd = grepl('-(mean|std)\\(', Titulo[,2])
Juntar1 = Juntar1[Mediastd]


Act = as.character(read.table("./Caso3/activity_labels.txt")[,2])
Juntar2 = Act[Juntar2]

cambio = colnames(Juntar1)
cambio = gsub("\\()", "", cambio)
cambio = gsub('([[:upper:]])', ' \\1', cambio)
cambio = gsub("Body Body", "Body", cambio)
cambio = gsub("\\-", " -", cambio)
cambio = gsub("-mean", "- Media", cambio)
cambio = gsub("-std", "- DesvEst", cambio)
cambio = gsub("t", "T", cambio)
cambio = gsub("f", "F", cambio)
colnames(Juntar1) = cambio

Sujeto = Juntar3
colnames(Sujeto) = "Sujeto"
Acción = Juntar2
Final = cbind(Sujeto, Acción, Juntar1)

library(dplyr) 
prom_final <- Final %>% group_by(Sujeto,Acción) %>% summarise_each(funs(mean))
write.csv(prom_final,"Promedio de acciones.csv")
write.csv(MediasFinal, "Medias acciones.csv")

write.csv(Final, "Datos Ordenados.csv")

