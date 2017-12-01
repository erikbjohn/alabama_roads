classification <- function(){
  project_location <- '~/Dropbox/pkg.data/alabama_roads/'
  classify.name <- 'roads_example1'
  f.dir <- paste0(project_location, 'raw/snapshots/')
  f.list.path <- list.files(f.dir, full.names = TRUE)
  f.list <- list.files(f.dir)
  
  dt.classify.location <- paste0(project_location, 'clean/dt.classify.', classify.name, '.rds')
  if(file.exists(dt.classify.location)){
    dt.classify <- readRDS(dt.classify.location)
    files.complete <- unique(dt.classify$fName)
  } else {
    files.complete <- ''
  }
  f.to.do <- data.table::data.table(f.name = f.list[which(!(f.list %in% files.complete))], f.path =f.list.path[which(!(f.list %in% files.complete))])
  
  labels.location <- '/tmp/output_labels.txt'
  graph.location  <- '/tmp/output_graph.pb'
  
  # Tensor head string 
  str.head <- 'source ~/tensorflow/bin/activate & ~/tensorflow/bazel-bin/tensorflow/examples/image_retraining/label_image'
  str.graph <- paste0('--graph=', graph.location)
  str.label <- paste0('--labels=', labels.location)
  str.tail <- '--output_layer=final_result:0 --image='
  str.block <- paste(str.head, str.graph, str.label, str.tail)
  randomPics <- sample(nrow(f.to.do), nrow(f.to.do), replace = FALSE)
  i <- 0
  for (pic in randomPics){
    i <- i + 1; cat(i, 'of', nrow(f.to.do), '\n')
    f.name <- f.to.do$f.name[pic]
    f.path <- f.to.do$f.path[pic]
    pano_id <- stringr::str_extract(f.name, stringr::regex('(?<=pano\\_id\\:).+(?=.+\\.jpg)'))
    type <- stringr::str_extract(f.name, stringr::regex('(?<=\\_)(lag|lead)(?=\\.jpg)'))
    classes <- system(paste0(str.block, f.path), intern=TRUE)
    l.classes <- sapply(classes, function(x) stringr::str_split(x, ' \\(score = '), simplify=TRUE)
    l.classes.dt <- lapply(l.classes, function(x) data.table::data.table(pano_id = pano_id, type =type, fName = f.name, category = x[1], score = x[2]))
    dt.classes.new <- data.table::rbindlist(l.classes.dt, use.names = TRUE, fill = TRUE)
    dt.classes.new <- dt.classes.new[category=='parks usage', category:='parks_usage']
    dt.classes.new$score <- as.numeric(stringr::str_replace_all(dt.classes.new$score, '\\)$', ''))
    knitr::kable(dt.classes.new)
    cat('\n')
    if(file.exists(dt.classify.location)){
      dt.classify <- data.table::rbindlist(list(dt.classify, dt.classes.new), use.names = TRUE)
      saveRDS(dt.classify, dt.classify.location)
    } else {
      saveRDS(dt.classes.new, dt.classify.location)
      dt.classify <- dt.classes.new
    }
  }
  dt.classify <- unique(dt.classify[, .(pano_id, type, fName, category, score)])
}