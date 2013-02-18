CreateEdgeList <- function(start.users,  # character vector of screenNames or IDs
                           index.csv,  # csv about users
                           edge.list.csv,  # csv where edgelist should be stored 
                           cainfo,  # where is SSL cerification file
                           number.to.check=length(start.users)){  # to definiate end


  ##################               #######################
  # TODO: handling errors in index.csv and edge.list.csv #
  #       solved by file.exists()                        #
  ##################               #######################

  # Checking inputs
  # start.users
  if(!is.character(start.users)){
    stop("start.users should be as character!")
    }
  # index.csv
  if(!is.character(index.csv) && !is.null(index.csv)){
    stop("index.csv should be as character, or NULL!")
    }
  # edge.list.csv
  if(!is.character(edge.list.csv)){
    stop("edge.list.csv should be as character!")
    }
  # cainfo
  if(!is.character(cainfo)){
    stop("cainfo should be as character!")
    }
  # number.to.check
  if(!is.numeric(number.to.check) || (number.to.check %% 1) != 0){
    stop("number.to.check should be integer!")
    }

  # Checking index.csv
  if(!is.null(index.csv)){
    if(!file.exists(index.csv)){
      write.table(data.frame(user="user", chkd="chkd"),
              file=index.csv,
              row.names=FALSE,
              append=false,
              col.names=FALSE,
              sep=",",
              quote=TRUE)
      warning("No index file found, new index file created.",
              call.=FALSE)
      }
    write.index <- TRUE
    }
    if(is.null(index.csv)){
      write.index <- FALSE
      }
    # Checking edge.list.csv
    if(!file.exists(edge.list.csv)){
      write.table(data.frame(source="source", target="target"),
              file=edge.list.csv,
              row.names=FALSE,
              append=FALSE,
              col.names=FALSE,
              sep=",",
              quote=TRUE)
      warning("No index file found, new edge list created.",
              call.=FALSE)
      }

  # Initiate the index:
  start.users.id <- lookupUsers(start.users, cainfo=cainfo)
  start.users.id <- laply(start.users.id, "[[", "id")
  if(write.index){
    user.index <- read.csv(index.csv)
    user.index <- rbind(user.index,data.frame(user=start.users.id[
                    !(start.users.id %in% as.character(user.index$user))
                    ],
                                              chkd=FALSE))
    } else{
    user.index <- data.frame(user=start.users.id, chkd=FALSE)
    index.csv <- "tmp.user.index.csv"
    }
  write.csv(user.index, file=index.csv)
  # clean memory
  rm(start.users.id, start.users)

  # raise Nmax with the number of users already checked
  number.to.check <- number.to.check + sum(user.index$chkd)
  # create a wariable which stores the number of checked users.
  checked <- sum(user.index$chkd)
  # clear user.index
  rm(user.index)

  # I cant use llply to methods ($getFriends),
  # so I will have to check one by one.
  # I could get more User,
  # but there is a smaller limit for friends and followers.
  while(checked < number.to.check){
    # read the index.csv
    user.index <- read.csv(index.csv)
    # get the earliest user and its friends and followers
    user <- getUser(users.index$user[min(which(user.index$chkd == FALSE))],
                    cainfo=cainfo)
    user.friend <- user$getFriendsIds(cainfo=cainfo)
    user.follower <- user$getFollowerIDs(cainfo=cainfo)
    # create edgelist and write to edge.list.csv
    out.edges <- data.frame(source=user$id,
                            target=user.friend)
    in.edges  <- data.frame(source=user.follower,
                            target=user$id)
    edge.list <- rbind(out.edges, in.edges)
    write.csv(edge.list, file=edge.list.csv, append=RUE)
    # clear edgelist from memory
    rm(edge.list, out.edges, in.edges)
    
    # update index.csv
    new.users <- unique(c(user.friend, user.follower))
    new.users <- new.users[new.users %in% as.character(user.index$user)]
    write.csv(data.frame(user=new.users, chkd=FALSE),
              file=index.csv,
              append=TRUE)
    # update the number of checked users
    checked <- sum(user.index$chkd)
    # clear memory:
    rm(new.users)

    # break the loop if we have checked evey user:
    if(!(sum(user.index$chkd == FALSE))){
      break()
      }
    }
  #return user.index if path is null
  if(!write.index){
    user.index <- read.csv(index.csv)
    unlink(index.csv)
    return(user.index)
    } else{
    print(paste("Index file at: ", index.csv,
                "Edgelist at: ", edge.list.csv, sep=""))
    }
  }