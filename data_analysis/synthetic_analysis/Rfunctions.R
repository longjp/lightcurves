####
#### useful R functions
####



####
####
####
dedupe = function(data.f,columns.separate){
  o1 = do.call(order,as.data.frame(data.f[,columns.separate]))
  data.f = data.f[o1,]
  duped = 1*duplicated(data.f[,columns.separate])
  row_id = rep(0,length(duped))
  for(i in 2:length(row_id)){
    row_id[i] = duped[i]*(row_id[i-1] + 1)
  }
  data.f$row_id = row_id
  return(data.f)
}









##########
########## testing area
##########
