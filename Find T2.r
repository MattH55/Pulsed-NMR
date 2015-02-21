# Pulsed-NMR
#Find T2
'findt2'<-function(filename,delay_time,num_pulse){
  x=read.csv(filename)
t=x[,4];
amp=x[,5];
xt=data.frame(t,amp)
t_max=t[which.max(amp)]
amp_max=max(amp)

print(t_max)
print(amp_max)

print(delay_time)



t_spk_est=vector(length=num_pulse)
n_spk_est=vector(length=num_pulse)
amp_spk_true=vector(length=num_pulse)
n_spk_true=vector(length=num_pulse)
t_spk_true=vector(length=num_pulse)
norm_amp_spk_true=vector(length=num_pulse)

#initspike=t_max+delay_time/1000;
for (k in 1:num_pulse){
  t_spk_est[k]=t_max+(k*2*delay_time)
  
  n_spk_est[k]=which.min(abs(t-t_spk_est[k]))
  print(n_spk_est[k])
  n1=n_spk_est[k]-3
  n2=n_spk_est[k]+3
  amp_spk_true[k]=max(amp[n1:n2])
  n_spk_true[k]=which.max(amp[n1:n2])
  t_spk_true[k]=t[n_spk_true[k]]
  norm_amp_spk_true[k]=amp_spk_true[k]/amp_max;
}
plot(t_spk_est,norm_amp_spk_true)

print(t_spk_est)

sumdat=data.frame(t_spk_est,norm_amp_spk_true)
len=nchar(filename)-4
print(len)
filename=substr(filename,1,len)
print(filename)
summary_filename=paste0(filename,'-summary.csv')
print(summary_filename)
write.csv(sumdat, summary_filename, row.names=FALSE)


}
  
