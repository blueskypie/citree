
#' run conditional inference tree with additional support
#'
#' The [partykit::ctree()] function only gives the best separation at each node,
#' i.e. one tree. This wrapper provides the following supports:
#'    * By setting `recursive = T`, all trees meeting p-val cutoff are produced
#'     and saved. Recursion is done by removing the 1st splitting variable from
#'     the df1 and running `runCtree()` inside `runCtree()`.
#'    * The info and stats of each node of each tree are collected and summarized
#'     in an excel file, which also contains ULRs to each tree.
#'    * Before running `partykit::ctree()`, low-informative columns and rows are
#'     removed to reduce computation and adjustment on association p-vals
#'
#' Note:
#'    * packages `partykit` and `openxlsx` are loaded, but not attached, in this function.
#'    * ctree uses `coin::independence_test()` to test the association of two
#'     variables of any data type. See [here](https://cran.r-project.org/web/packages/coin/vignettes/LegoCondInf.pdf)
#'     for theory behind the test, and [here](https://stats.stackexchange.com/questions/404589/ctree-in-r-how-optimal-is-the-optimal-split-point)
#'     for an explanation of the algorithm.
#'
#' @param cohort char; name of the observation cohort as an annotation in the drawn tree
#' @param df1 data.frame; columns are variables and rows are observations
#' @param oDir char; output directory for the tree plot and a summary excel file;
#'    * one pdf file for each tree
#'    * each file is named as `paste0(oDir,.Platform$file.sep, cohort,'.',yName,'.',gList$counter,'.pdf')`
#'    * The excel file is the content of `stats` from the @return (see below), and
#'     is named as `paste0(oDir,.Platform$file.sep,cohort,'.xlsx')`
#' @param yi int; index of y variable
#' @param pCut p-val for significant association; not adjusted.
#' @param recursive logical;
#'    * F: only produce the best tree
#'    * T: produce all trees meeting `pCut`
#' @param getReturn logical; if T, return a list below; no returns otherwise.
#'   it's also used to reduce the internal data transfer load if `recursive = T`.
#' @param ctrlParas list; parameters for [partykit::ctree_control()]
#' @param naParas list; parameters for [rmNA()]
#' @param nzvParas list; parameters for [rmNZV()]
#' @param gList a listenv list; it's for internal recursion tracking; users
#'   should ignore this argument.
#'
#' @return if `getReturn`, a list of following items; none otherwise.
#'    * `df`: cleaned df1; NA if df1 has only one column or < 10 rows with y values.
#'    * `stats`: possible values:
#'      + NA `ctree()` doesn't run due to one of the following reasons:
#'        - only one column in `df1`
#'        - < 10 rows where y is not NA
#'        - y has low variance and is removed by `rmNZV()`
#'        - no tree fitting the `pCut` is found. In this case, try increasing `pCut`
#'      + A data.frame of following columns, one tree per row
#'        - counter: the index of each tree drawing
#'        - cohort, y, pCutoff
#'        - spVar1,pVal1: the name and p-val of the splitting variable at node 1
#'        - nNode: number of nodes of the tree
#'        - spVars: a string containing the names and stats of all splitting
#'            variables. for each node, the format is "name,p-val,cut,gtOnRight"
#'            nodes are separated by ';'.
#'        - plot: the path to the tree plot
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom stats as.formula
#'
#' @examples # none
runCtree=function(df1,cohort,oDir,yi=1,pCut=0.05,
                  recursive=T, getReturn=T,
                  ctrlParas=list(minsplit= 10,minbucket = 5,maxsurrogate=2,
                                 alpha = pCut),
                  naParas=list(margins=1,maxNA.perc=0.95, minNonNA.count=5),
                  nzvParas=list(minUniPerc = 0.05,minUniCount=5),
                  gList=NULL){

  ret=list(df=NA,stats=NA)
  run1=F

  if(is.null(gList)) {
    run1=T
    # use a listenv list since it's passed by reference for recursion.
    gList=listenv::listenv()
  }

  if((is.matrix(df1) || is.data.frame(df1)) && ncol(df1)>1) {

    if(yi!=1){
      k=setdiff(1:ncol(df1),yi)
      df1=df1[,c(yi,k)]
      yi=1
    }

    yName=colnames(df1)[yi]
    rInds=which(!is.na(df1[[yi]]))

    if(length(rInds)>9){
      #remove low-informative columns and row to reduce computation and
      # adjustment on association p-vals
      df2=do.call(rmNA,c(list('df1'=df1[rInds,]),naParas))
      df2=do.call(rmNZV,c(list('df1'=df2),nzvParas))
      # df2=rmNAs(df1[rInds,],2,naCut)
      # df2=rmNZV(df2)

      for (i in 1:ncol(df2)) {
        # ctree cannot deal with char columns
        if(is.character(df2[[i]]))
          df2[[i]]=factor(df2[[i]],levels = sort(setdiff(unique(df2[[i]]),NA)))
      }

      if(yName %in% colnames(df2)){
        ret$df=df2
        cRe=partykit::ctree(as.formula(paste0(yName,' ~ .')),data = df2,
                            control = do.call(partykit::ctree_control,ctrlParas))
        # partykit::ctree_control(minsplit= 10,minbucket = 5,maxsurrogate=2, alpha = pCut)
        reList=ctree2splitInfo(cRe,pCut)

        if(!is.null(reList$spVars.str)){
          if(!dir.exists(oDir)) {
            dir.create(oDir)
            warning(oDir,' does not exist and is created!\n')
          }

          if(length(gList)==0){
            cNames=c('counter','cohort','y','pCutoff','pVal1','spVar1','nNode','spVars','plot')
            gList$statDf=data.frame(matrix(NA,nrow=ncol(df1),ncol = length(cNames)))
            colnames(gList$statDf)=cNames

            gList$counter=1
          }

          nNode=length(reList$pVals)
          pVal1=reList$pVals[[1]][1] #splitting p-val at node 1
          spVar1=names(reList$pVals[[1]])[1]

          ofName=paste0(cohort,'.',yName,'.',gList$counter,'.pdf')
          # 'counter','cohort','y','pCutoff','pVal1','spVar1','nNode','spVars','plot'
          gList$statDf[gList$counter,]=list(gList$counter,cohort,yName,pCut,pVal1,
                                            spVar1,nNode,reList$spVars.str,ofName)

          ofName=paste0(oDir,.Platform$file.sep,ofName)

          pdf(ofName,width = 5+1.2*nNode,height = 5+nNode)
          plot(cRe,main=paste0(yName, '\n(cohort:',cohort,'; N=',nrow(df2),')'))
          dev.off()

          gList$counter=gList$counter+1
          if(recursive){
            ind=which(colnames(df2)==spVar1)
            stopifnot(length(ind)==1)
            if(ncol(df2)>2){
              df2=df2[,-ind]
              runCtree(df2,cohort,oDir,pCut=pCut,getReturn=F,gList = gList)
            }
          }
          if(run1) ret$stats=rmNAs(gList$statDf)
        }else if(is.null(gList$counter)){
          warning('no tree fitting the pCut is found for ', yName, '; try increasing pCut!')
        }
      }
    }
  }

  if(run1 && is.data.frame(ret$stats)){
    addURL2df(ret$stats,'plot','plot',fName = paste0(oDir,.Platform$file.sep,cohort))
  }

  if(getReturn) return(ret)
}




#' add URLs to one column of a data.frame
#'
#' Note package `openxlsx` is loaded but not attached in this function.
#'
#' @param df1 data.frame
#' @param textColm int or char; the column to display the text of the URL
#' @param urlColm int or char; the column of the URL
#' @param fromRow int; the starting row
#' @param fName char; output file name
#' @param ... passed to [openxlsx::saveWorkbook()]
#'
#' @return none by default; see the return of [openxlsx::saveWorkbook()] for
#'   details.
#' @export
#'
#' @examples # none
addURL2df=function(df1,textColm, urlColm,fromRow = 1, fName=NULL,...){

  k = openxlsx::makeHyperlinkString(text = df1[fromRow:nrow(df1),textColm],
                                    file = df1[fromRow:nrow(df1),urlColm])

  if(is.character(textColm)) textColm=match(textColm,colnames(df1))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, sheet = 1, x = df1)
  openxlsx::writeFormula(wb, sheet = 1, startRow = fromRow+1, startCol = textColm, x = k)
  if(!is.null(fName))
    openxlsx::saveWorkbook(wb,paste0(fName,'.xlsx'),...)
}




#' get info of significant variables in each node of a ctree
#'
#' see @return for details. Note that package `partykit` is loaded, but not
#' attached, in this function.
#'
#' @param cRe object returned from [partykit::ctree()]
#' @param pCut numeric; p-val cutoff to be significant
#' @param nodeInds int vectors, e.g. `1:5` for a tree of five nodes, including leaves
#'
#' @return return a list of following items:
#'    * `pVals` is a list of significant variables at each node of `nodeInds` excluding leaves;
#'      - `length(pVals)`: number of internal nodes
#'      - `names(pVals)`: the name of splitting variable at each node
#'      - each item of `pVals` is a numeric vector of p-vals for all variables
#'       whose `p-val <= pCut`, ordered by increasing p-vals
#'    * `spVars.df` a data.frame of following four columns, one node per row,
#'      - `variable`: the name of splitting variable (`spVar`)
#'      - `pVal`: the p-val of the `spVar`
#'      - `sCut`: the splitting point
#'      - `gtOnRight`: T/F; are observations of greater (`gt`: greater than) values
#'       on the right branch of the node? The separation of numeric spVar is always
#'       `<=` and `>`; if the leaves are listed in increasing `y`, `gtOnRight==T`
#'       means the `spVar` has positive association with `y`.
#'    * `spVars.str`, a string combining all rows of `spVars.df`. rows are
#'      separated by ';' and cells are by ','
#'
#'  if cRe has only one root node, i.e. no splitting, NULL is returned
#' @export
#'
#' @examples # none
ctree2splitInfo=function(cRe,pCut,nodeInds = partykit::nodeids(cRe, terminal = F)){
  pList=spVarList=list()
  spVars.df=spVars.str=NULL
  partykit::nodeapply(cRe, ids = nodeInds, function(x) {
    if(!partykit::is.terminal(x)){
      node1=partykit::info_node(x)
      inds=which(node1$criterion['p.value',]<=pCut)
      if(length(inds)>0){
        pVals=node1$criterion['p.value',][inds]
        names(pVals)=colnames(node1$criterion)[inds]
        pVals=sort(pVals)
        # the 1st item may not be the spVar if p-val ties.
        spVar=names(node1$p.value)
        if(spVar!=names(pVals)[1]){
          spInd=match(spVar,names(pVals))
          inds=setdiff(1:length(pVals),spInd)
          pVals=pVals[c(spInd,inds)]
        }

        pList[[spVar]]<<-pVals
      }
      spVarList[[spVar]]<<-list(variable=names(pVals)[1],
                                pVal=round(pVals[1],4),
                                sCut=ifelse(is.null(x$split$breaks),NA,x$split$breaks),
                                gtOnRight=x$split$right)
    } })

  if(length(spVarList)>0){
    for (i in spVarList) {
      k=NULL
      for (j in i) {  k=ifelse(is.null(k),j,paste(k,j,sep = ','))     }
      spVars.str=ifelse(is.null(spVars.str),k,paste(spVars.str,k,sep = '; '))
    }
    #browser()
    spVars.df=do.call(rbind.data.frame, spVarList)
  }

  list(pVals=pList,spVars.df=spVars.df, spVars.str=spVars.str)
}



#' remove rows or columns with missing values in a data.frame or matrix
#'
#' @param df1 data.frame or matrix
#' @param margins an int vector of 1 and 2 for rows and columns;
#'   e.g. 2:1 means the removal order is column then row.
#' @param maxNA.perc,minNonNA.count maximum %NA or minimum non-NA items allowed
#'   failure to match either leads to removal
#'
#' @return `df1` after cleaning; data type is kept even if 0 or 1 row or column remains
#' @export
#'
#' @examples # none
rmNA=function(df1,margins=1,maxNA.perc=0.95, minNonNA.count=5){
  d1=dim(df1)
  isM=is.matrix(df1)
  if(isM) df1=as.data.frame(df1) #in order to use df1[,-rmInds,F]

  for(i in margins){
    naPercs=apply(df1, i, \(x) sum(is.na(x))/length(x))
    nonNA.counts=apply(df1, i, function(x) sum(!is.na(x)))
    rmInds=which(naPercs>maxNA.perc | nonNA.counts < minNonNA.count)
    if(length(rmInds)>0){
      #if rmInds includes all rows or/and columns, still returns a data.frame of 0 row or/and column
      # df1[,-rmInds,F] can only be used on df, not matrix
      df1=`if`(i==1,df1[-rmInds,,F],df1[,-rmInds,F])
    }
  }
  if(isM) df1=as.matrix(df1)

  k=d1 - dim(df1)
  if(max(k)>0){
    k=paste(k,c('rows','columns'), collapse = ' and ')
    warning(k,' are removed by rmNAs()!')
  }

  df1
}



#' remove columns of near zero variance in a data.frame or matrix.
#'
#' It applies to any data type whereas [caret::nearZeroVar()] is only for numeric columns.
#'
#' @param df1 a data.frame or matrix
#' @param minUniPerc,minUniCount criteria to remove columns
#'   unique values are all the values except the most common value, e.g. `1,4,2` in `c(1,2,3,3,4)`.
#'    `uniCount` and `uniPerc` are the count and percentage of samples having the unique values
#'    failure to match either leads to removal
#'
#' @return a cleaned `df1`; data type is kept even if 0 or 1 column remains
#' @export
#'
#' @examples # none
rmNZV=function(df1,minUniPerc = 0.05,minUniCount=5){
  ynVec=sapply(as.data.frame(df1), function(x){
    k=sort(table(x))
    if(length(k)<=1){
      return(F)
    }
    uniCount=sum(k[-length(k)]) #k[length(k)] is the most common value
    uniPerc=uniCount/sum(k)
    uniCount >= minUniCount && uniCount/sum(k) >= minUniPerc
  })

  df2=df1[,names(which(ynVec))]

  k=dim(df1) - dim(df2)
  if(max(k)>0){
    k=paste(k,c('rows','columns'), collapse = ' and ')
    warning(k,' are removed by rmNZV()!')
  }

  df2
}
