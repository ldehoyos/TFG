### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       Once all the data has been created, 
#         the APC calculated and 6 group-matrices have
#         been created (2 sex groups, 3 age groups)
#         the files are copied into the FinishedFiles
#         folder.
#
#       These are the files that will be used on the 
#         following steps.
#
# ===================================================== 

setwd('D:/lucia_TFG/01-PreparedFiles/')
fnamesF= list.files(pattern = 'S0.csv')
fnamesM= list.files(pattern = 'S1.csv')

fnamesF2 = paste0('D:/lucia_TFG/02-FinishedFiles/', gsub('_S0.csv', '.csv', gsub('scale', 'S', gsub('thickness_APC_group', 'CT_F_G', fnamesF))))
fnamesM2 = paste0('D:/lucia_TFG/02-FinishedFiles/', gsub('_S1.csv', '.csv', gsub('scale', 'S', gsub('thickness_APC_group', 'CT_M_G', fnamesM))))


file.copy(from= fnamesF, to = fnamesF2)
file.copy(from= fnamesM, to = fnamesM2)

file.copy("D:/lucia_TFG/01-PreparedFiles/fenotypes_APC.csv", "D:/lucia_TFG/02-FinishedFiles/covars.csv")
