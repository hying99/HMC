#####predict.labels2是未经DAGlabel处理的
#####y是经DAGlabel处理的
#####此程序为比对用DAGlabel是否做到让分类结果真正满足层级约束要求
violatelabels <- vector()
noeffectlabels <- vector()
for (i in 1:nrow(predict.labels2)) {
  for (j in 1:ncol(predict.labels2)) {
    ###找两者不同的pair
    if(predict.labels2[i,j] != y[i,j])
    {
      violatelabels <- c(violatelabels,paste(i,j,sep = ",",collapse =" "))
    ###让test.select.labels判断谁对  
      m <- i
      n <- j
      if(y[m,n] != test.select.table2[m,n])
      {
        noeffectlabels <- c(noeffectlabels,paste(m,n,sep = ",",collapse = " "))
      }
    }
  }
}
####经过DAGlabel处理前后分类不同的标签有violatelabels个，DAGlabel处理之后反而给分错的有noeffectlabels个####
####DAGlabel分对的标签####
commonpairs <- violatelabels[-which(violatelabels %in% noeffectlabels)]
positivepairs1 <- vector()
positivepairs0 <- vector()
for (k in 1:length(commonpairs)) {
  pair <- vector()
  pair <- as.numeric(unlist(strsplit(commonpairs[k],",")))
  ####DAGlabel起作用的是当自身==0，而nodes.to.descendants[[,]] == 1的情况####
  if(y[pair[1],pair[2]] == 1)
  {
      if( 1 %in% predict.labels2[pair[1],nodes.to.descendants2[[pair[2]]]])
      {
        positivepairs1 <-  c(positivepairs1,paste(pair[1],pair[2],"WORK!",sep = ",",collapse = " "))
      }
      else
      {
        positivepairs1 <-  c(positivepairs1,paste(pair[1],pair[2],"NOT WORK!",sep = ",",collapse = " "))
      }
  }
  else
  {
    if( 0 %in% predict.labels2[pair[1],nodes.to.ancestors2[[pair[2]]]])
    {
      positivepairs0 <-  c(positivepairs0,paste(pair[1],pair[2],"WORK!",sep = ",",collapse = " "))
    }
    else
    {
      positivepairs0 <-  c(positivepairs0,paste(pair[1],pair[2],"NOT WORK!",sep = ",",collapse = " "))
    }
  }
}
####DAGlabel处理之后分对的有commonpairs个，其中，分成了正类的有positivepairs1个，DAGlabel真正起作用的positivepairs1里会有WORK的字样，否则就是没起作用####



#####此后的代码为了看predict.labels2里面，哪个pair应当被修正#####

tochangelabels <- vector()
for (u in 1:nrow(predict.labels2)) 
  {
  for (v in 1:ncol(predict.labels2))
    {
  #   if(predict.labels2[u,v] == 0 && test.select.table2[u,v] == 1)
  #   {
  #     tochangelabels <-  c(tochangelabels,paste(u,v,sep = ",",collapse = " "))
  #   } 
    
      if( FALSE %in% is.na(nodes.to.descendants2[[v]])  ){
    for (q in 1:length(nodes.to.descendants2[[v]])) 
    {
      if(predict.labels2[u,v] == 0 && predict.labels2[u,nodes.to.descendants2[[v]][q]] == 1)
      {
        tochangelabels <-  c(tochangelabels,paste(u,v,sep = ",",collapse = " "))
      }
    }
    }
  }
}
#####该被修正的数量是tochangelabels





