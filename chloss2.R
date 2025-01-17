####实现chloss的程序文件 其中pi直接取为svm的输出结果
####20181218


# #输入带有根结点的节点列表，根结点的序号为0
# total.index.ch = MakeIndex(select.node, include.root = TRUE)
# nodes.to.index.ch = total.index.ch[[1]]
# nodes.to.children.ch = total.index.ch[[2]]
# nodes.to.ancestors.ch = total.index.ch[[3]]
# nodes.to.parents.ch = total.index.ch[[4]]
# nodes.to.descendants.ch = total.index.ch[[5]]


# #### 将SVM的概率结果读入
# file.middle = "0"
# file.type = ""

#设置mat文件存储路径
#setwd(paste(data.path,"//matfile",sep = ""))
library(R.matlab)
aa <- 1
datapath <- paste("dataset",aa,sep = "")
setwd(paste("C:/Users/1231/Desktop/dataprocessing/data/",datapath,sep = ""))
mat.file=("newdataset_decision.mat")
probability.data2=readMat(mat.file,fixNames = FALSE)
#prob.for.genes  存储时，对每个节点来说，第一位是为label1的概率，而后是为0的概率
prob.for.genes2=probability.data2$decision_test
w1=1
w3=1
w2=2
w4=2
c.matrix=matrix(0,nodes.total.num,1)
c.matrix[1,1]=1

go.for.level.index2 <- vector("list",length = length(go.for.level2))
for (j in 1:length(go.for.level.index2)) {
  go.for.level.index2[[j]] <- which(except.root.labels2 %in% go.for.level2[[j]])
}

#计算Ci,该值仅与节点有关，对每个样本均相同
for(k in 1:length(go.for.level.index2))
{
  for (i in 1:length(go.for.level.index2[[k]])) {
    if( k == 1)
    {
      sibling.num2=each.level.nodes.num2[[1]]
      c.matrix[((go.for.level.index2[[1]][i])+1),1]=1/sibling.num2
    }
    else
    {
      parentnode.num2=length(nodes.to.parents2[[except.root.labels2[go.for.level.index2[[k]][i]]]])
      current.c=0
      for(j in 1:parentnode.num2)
      {
        parentnode.index2=nodes.to.parents2[[except.root.labels2[go.for.level.index2[[k]][i]]]][j]
        sibling.num2=length(nodes.to.children2[[except.root.labels2[parentnode.index2]]])
        #current.c=current.c+c.matrix[parentnode.index2,1]/sibling.num2
        current.c=current.c+c.matrix[(parentnode.index2+1),1]/sibling.num2
      }
      # c.matrix[((go.for.level.index2[[k]][i])+1),1] = current.c
    c.matrix[((go.for.level.index2[[k]][i])+1),1]=current.c/parentnode.num2
    }
    }
  }
###变换节点函数只需要这一块注释/拿走注释
c.matrix=matrix(1,nodes.total.num,1)
#提取各节点为1的概率矩阵
prob.is.one=matrix(0,nrow(prob.for.genes2),(nodes.total.num-1))
for(i in 1:nrow(prob.for.genes2))
{
  for(j in 1:(nodes.total.num-1))
  {
    prob.is.one[i,j]=prob.for.genes2[i,(2*j-1)]
  }
}


#计算pi
p.matrix=cbind(matrix(1,nrow(prob.for.genes2),1),prob.is.one)


sigma.one=matrix(0,nrow(prob.for.genes2),nodes.total.num)
sigma.two=matrix(0,nrow(prob.for.genes2),nodes.total.num)
#求解sigma(1)
for(k in 1:nrow(prob.for.genes2))
{
  for(i in 1:nodes.total.num)
  {
    cur.node=select.nodes2[i]
    is.leafnode=cur.node %in% go.leaf.nodes2
    if(is.leafnode==TRUE)
    {
      sigma.one[k,i]=0
    } else
    {
      children.index=nodes.to.children2.ch[[select.nodes2[i]]]
      children.num=length(children.index)
      inter.value=0
      for(j in 1:children.num)
      {
        inter.value=inter.value+ c.matrix[(children.index[j]+1),1]*p.matrix[k,(children.index[j]+1)]
      }
      sigma.one[k,i]=(w2-w1)*inter.value
    }
  }
}

#求解sigma(2)
for(k in 1:nrow(prob.for.genes2))
{
  for(i in 1:(nodes.total.num))
  {
    cur.node.index=i-1
    if(nodes.to.index2.ch[i]==0)
    {
      sigma.two[k,i]=0
      
    }else
    {
      parents.index=nodes.to.parents2.ch[[i]]
      parents.num=length(parents.index)
      inter.value=1
      for(j in 1:parents.num)
      {
        inter.value=inter.value*p.matrix[k,(parents.index[j]+1)]
      }
      value.one=w1*c.matrix[i,1]*p.matrix[k,i]*parents.num
      value.two=w3*c.matrix[i,1]*parents.num*(inter.value-p.matrix[k,i])
      value.three=w4*c.matrix[i,1]*parents.num*(1-inter.value)
      sigma.two[k,i]=value.one-value.two-value.three
    }
  }
}

sigma=sigma.one+sigma.two

