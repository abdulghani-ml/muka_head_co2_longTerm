#### ENERGY BALANCE ####
input <- df_merge_month$RN - df_merge_month$SH - df_merge_month$SLE
output <- df_merge_month$LE + df_merge_month$H

output[which (output < -300)] <- NA
input[which (input < - 50)] <- NA


plot(input,output, ylab = "LE + H", xlab = "RN - SLE - SH") 


EB_Ratio <- sum(output, na.rm=TRUE)/sum(input,na.rm=TRUE)
