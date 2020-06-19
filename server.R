library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(readr)
library(DT)
library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)
library(BiocManager)
options(repos = BiocManager::repositories())
library(Biostrings)
library(plotly)
library(circlize)
library(DECIPHER)

library(viridis)

eli1 <- read_delim("Matching_between_V1_V2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
eli <- read.delim("RAP-MSU_2020-03-24.txt", header=FALSE)

protDownload<-readAAStringSet("covid_prot.fa")
nucDownload<-readAAStringSet("covid_nuc.fasta")

supported_msa_class <- c("DNAStringSet",  "RNAStringSet", "AAStringSet", "BStringSet",
                         "DNAMultipleAlignment", "RNAMultipleAlignment", "AAMultipleAlignment",
                         "DNAbin", "AAbin")


prepare_msa <- function(msa) {
    if (methods::missingArg(msa)) {
        stop("no input...")
    } else if (methods::is(msa, "character")) {
        msa <- seqmagick::fa_read(msa)
    } else if (!class(msa) %in% supported_msa_class) {
        stop("multiple sequence alignment object no supported...")
    }
    
    res <- switch(class(msa),
                  DNAbin = DNAbin2DNAStringSet(msa),
                  AAbin = AAbin2AAStringSet(msa),
                  DNAMultipleAlignment = DNAStringSet(msa),
                  RNAMultipleAlignment = RNAStringSet(msa),
                  AAMultipleAlignment = AAStringSet(msa),
                  msa ## DNAstringSet, RNAStringSet, AAString, BStringSet
    )
    return(res)
}


tidy_msa <- function(msa, start = NULL, end = NULL) {
    aln <- prepare_msa(msa)
    alnmat <- lapply(seq_along(aln), function(i) {
        base::strsplit(as.character(aln[[i]]), '')[[1]]
    }) %>% do.call('rbind', .)
    ## for DNAbin and AAbin
    ## alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
    alndf <- as.data.frame(alnmat, stringsAsFactors = F)
    
    alndf$name = names(aln)
    
    cn = colnames(alndf)
    cn <- cn[!cn %in% "name"]
    df <- gather(alndf, "position", "character", cn)
    
    y <- df
    y$position = as.numeric(sub("V", "", y$position))
    y$character = toupper(y$character)
    
    y$name = factor(y$name, levels=rev(names(aln)))
    
    
    if (is.null(start)) start <- min(y$position)
    if (is.null(end)) end <- max(y$position)
    
    y <- y[y$position >=start & y$position <= end, ]
    
    y$character<-as.factor(y$character)
    return(y)
}

server <- shinyServer(function(input, output, session) {
    
    data <- reactive({ 
        req(input$file1) 
        
        inFile <- input$file1
        
        
        if (str_detect(inFile$datapath, ".xls")) {
            df<-read_excel(inFile$datapath)
        }else{
            df<-read.csv(inFile$datapath, header = input$header, sep = input$sep)
            
        }
        return(df)
    })
    output$data<-DT::renderDataTable( data(),filter="top")
    
    
    CleanData<-reactive({
        df<-data()
        x<-names(df)
        dd<-separate_rows(df,x[2],sep = ",")
        dd<-separate_rows(dd,x[2],sep = ";")
        return(dd)
    })
    
    output$CleanData<-DT::renderDataTable( DataFinal(),filter="top")
    
    DataFinal<-reactive({
        df<-CleanData()
        if (input$v1 !="" & input$v2!= "") {
            names(df)<-c(input$v1,input$v2)
            return(df)
        }
        return(df)
    })
    
    DataNa<-reactive({
        df<-DataFinal()
        n<-names(df)
        d1<-df[df[,1] == "None", ]
        d2<-df[df[,2] == "None", ]
        v1<-dim(d1)
        v2<-dim(d2)
        
        data<-data.frame(v1[1],v2[1])
        names(data)<-c("colonne1", "colonne2")
        if (input$v1 !="" & input$v2!= "") {
            names(data)<-c(input$v1 , input$v2)
            return(data)
        }else{
            
            return(data)
        }
        
        
    })
    
    output$DataNa1<-renderTable({
        return(DataNa())
    })  
    
    output$NaText1<-renderText({
        n<-DataNa()
        nam<-names(n)
        paste0 (n[1,1] ," identifiers do not have genes that match them in the column ",nam[2])
    })
    
    output$NaText2<-renderText({
        n<-DataNa()
        nam<-names(n)
        paste0( n[1,2] ," identifiers do not have genes that match them in the column ",nam[1])
    })
    
    output$StrData<-renderPrint({
        str(DataFinal())
    })
    DataRetour<-reactive({
        
        df<-DataFinal()
        id<-input$ID
        id<-as.data.frame(id)
        id<-separate_rows(id,"id",sep = ",")
        d1<-data.frame()
        for(i in id[,1])
            if (i %in%  df[,1]) {
                d1 <- rbind(d1,unique(df[df[,1] == i, ]))
            }else{
                d1 <- rbind(d1,unique(df[df[,2] == i, ]))
            }
        
        return(d1)
    })
    
    output$IDRetour<-renderTable({
        require(input$ID)
        id<-input$ID
        id<-as.data.frame(id)
        id<-separate_rows(id,"id",sep = ",")
        df<-DataRetour()
        if ( id[1,1] %in%  df[,2]) {
            df<-df[,c(2,1)]
            return(df)
        }
        return(df)
    })
    
    
    output$retour<-renderPrint({
        require(input$ID)
        df<-DataRetour()
        id<-input$ID
        id<-as.data.frame(id)
        id<-separate_rows(id,"id",sep = ",")
        if (input$ID!= "") {
            
            if ( id[1,1] %in%  df[,1]) {
                return(as.character(unique(df[,2])))
            }
            return(as.character(unique(df[,1])))
        }
        
    })
    
    
    
    
    
    ##########################•fastainput
    
    
    fastadata<-reactive({
        req(input$fasta) 
        inFile <- input$fasta
        if (input$type == 'BStringSet') {
            df<- readBStringSet(inFile$datapath)
        }else if (input$type == 'DNAStringSet'){
            df<- readDNAStringSet(inFile$datapath)
        }else if (input$type == 'RNAStringSet') {
            df<- readRNAStringSet(inFile$datapath)
        }else{
            df<- readAAStringSet(inFile$datapath)
        }
        return(df)
    })
    
    output$fastadata1<-renderPrint({
        req(input$fasta) 
        x<-fastadata()
        return(x)
    })
    
    
    #########################msa
    
    
    
    DataMsa<-reactive({ 
        req(input$fasta) 
        df<-fastadata()
        df<-tidy_msa(df)
        df<-as.data.frame(df)
        return(df)
        
    }) 
    
    output$DataMsa<-DT::renderDataTable(DataMsa(),filter="top")
    
    output$MsaPlot<-renderPlot({
        req(input$fasta) 
        df<-DataMsa()
        df$name<-str_extract(df$name,"....[:digit:]......")
        if (input$START == 1 & input$END == 1000) {
            data<-df[1:1000,]
        }else{
            data<-df[input$START:input$END,]
        }
        
        ggheatmap <- ggplot(data, aes(position, name, fill =character))+
            geom_tile(color = "white")+
            geom_text(aes(position, name, label = character), color = "black", size = 4)+
            theme_minimal()+ # minimal theme
            
            coord_fixed()
        ggheatmap
    })
    
    
    output$FastaHist<-renderPlotly({
        df<-DataMsa()
        fig <- plot_ly(
            type='histogram',
            x=~ df$character)
        
        
        fig <- fig %>% layout(
            barmode="overlay",
            bargap=0.1)
        
        fig
    })
    
    #######################Download button
    
    
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(eli, file)
        }
    )
    
    
    output$downloadData1 <- downloadHandler(
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(eli1, file)
        }
    )
    
    
    ###################msa Download botton
    
    ####covid_prot
    output$downloadData2 <- downloadHandler(
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(protDownload, file)
        }
    )
    
    ######covid_nuc
    output$downloadData3 <- downloadHandler(
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(nucDownload, file)
        }
    )
    
    ##################circular plot
    
    output$cir<-renderPlot({
        df<-DataMsa()
        n = nrow(df)
        df = data.frame(factors = sample(df$character, n, replace = TRUE),
                        x = rnorm(n), y = runif(n))
        circos.par("track.height" = 0.1)
        circos.initialize(factors = df$factors, x = df$x)
        
        
        
        circos.track(factors = df$factors, y = df$y,
                     panel.fun = function(x, y) {
                         circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"), 
                                     CELL_META$sector.index)
                         circos.axis(labels.cex = 0.6)
                     })
        col = rep(c("#FF0000", "#00FF00"), 16)
        circos.trackPoints(df$factors, df$x, df$y, col = col, pch = 16, cex = 0.5)
        
        #circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)
        bgcol = rep(c("#78bef0", "#68e3db"), 16)
        circos.trackHist(df$factors, df$x, bin.size = 0.2, bg.col = bgcol, col = NA)
        
        circos.track(factors = df$factors, x = df$x, y = df$y,
                     panel.fun = function(x, y) {
                         ind = sample(length(x), 10)
                         x2 = x[ind]
                         y2 = y[ind]
                         od = order(x2)
                         circos.lines(x2[od], y2[od])
                     })
        
        
        
        circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
            xlim = CELL_META$xlim
            ylim = CELL_META$ylim
            breaks = seq(xlim[1], xlim[2], by = 0.1)
            n_breaks = length(breaks)
            circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
                        breaks[-1], rep(ylim[2], n_breaks - 1),
                        col = rand_color(n_breaks), border = NA)
        })
        
        
        
    })
    
    
    
    
    
    
    
    #####parwaise
    content<-reactive({
        df<-fastadata()
        s1<-df[[input$nb1]]
        s2<-df[[input$nb2]]
        alg<-pairwiseAlignment(s1,s2)
        
        seq <- c(alignedPattern(alg), alignedSubject(alg))
       
        p<-BrowseSeqs(seq,openURL=FALSE)
        p
        
    })
    getPage<-function() {
        return(includeHTML( file (content() )))
    }
    output$inc<-renderUI({
        
            getPage()
        
    })
    
    
    #donuts 
    
    output$donuts<-renderPlotly({
        df<-DataNa()
       if (df[1,1]!=0) {
           
      
        col<-colnames(df)
        df<-t(df)
        df<-as.data.frame(df)
        df$name<-col
        names(df)<-c("v2","v3")
        fig <- df %>% plot_ly(labels = ~df[,2], values = ~df[,1])
        fig <- fig %>% add_pie(hole = 0.6)
        fig <- fig %>% layout(title = "Missing Values",  showlegend = F,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
       } 
    })
    
    
    
    
  
    
    
    
    
    
    
})
