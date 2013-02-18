# Twitter robot
First, I had to found tools to manage http connection:

```{r loading libraries}
library(RCurl)
library(XML)
```

Found some useful tips [here] [tips], then I tried them:
  [tips]: http://ademar.name/blog/2006/04/curl-ssl-certificate-problem-v.html "about ssl problem"

##  Autehtication issue
The aforementioned site told me where I could get the SSL certificate, so I downoaded it:
```{r download the SSL certificate}
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="doga/crawler/cacert.perm")
```

Now, the only thing I had to do just write some code. Then I've found the [manual of the twitteR package] [twitteR]
  [twitteR]: http://cran.r-project.org/web/packages/twitteR/twitteR.pdf 


## Creating API

it needs login....

try twitteR package (Friend = Following) source: [twitteR.pdf] [twitteR]

```{r register my Twitter API after registrating it on Twitter}
# library(twitteR)
# library(ROAuth)
# Auth <- OAuthFactory$new(consumerKey="XXX",
#                        consumerSecret="XXX",
#                         requestURL="https://api.twitter.com/oauth/request_token",
#                         accessURL="https://api.twitter.com/oauth/access_token",
#                         authURL="https://api.twitter.com/oauth/authorize")
# Auth$oauthKey="XXX"
# Auth$oauthSecret="XXX"
# Auth$handshake(cainfo="doga/crawler/cacert.perm")
# save(Auth,file="doga/crawler/Auth.api")
```
 API created, lets try load Friends and Followers.
*note:* **every** get/update need the cainfo argument 

```{r loading packages, defining defaults}
library(plyr)
library(RCurl)
library(XML)
library(twitteR)
library(ROAuth)
## basic files:
#  certification:
cacert <- "doga/crawler/cacert.perm"
#  outfile of edgelist:
Outfile1 <- "doga/crawler/followedgelist.csv"
#  outfile of userindex:
Outfile2 <- "doga/crawler/userindex.csv"
#  API
APIfile <- "doga/crawler/Auth.api"

## Activate account:
registerTwitterOAuth(get(load(APIfile)))
```
# Algorithm:

1.:
  - start from own account, collect Friends and Followers
  - create an edgelist (data.frame) from the data
  - create a data.frame with unique screen_names and checkbox: already read.

2.: do the first on the unique df with lookupUsers(checkbox==FALSE)
  - check checkboxes

3.:  Save the datas, clear memory. Likely it will use a lot of storage.

 REPEAT: Until the max 1 million user

```{r function to get connections of a Twitter User}
followerList <-  function(User = NULL, #string: screen_name or id of a user
                           CAInfo="doga/crawler/cacert.perm"){ #string: certificate file name

# This function returns a data.frame edgelist
# of the followers and friends of the User

  if(!require(twitteR) |
     !require(ROAuth)){
    stop("Please check packages ROAuth and twitteR !")
    }

  # get the friends and followers of startUser
  U <- getUser(User,cainfo=CAInfo)
  # get IDs
  friends   <- U$getFriendIDs(cainfo=CAInfo)
  followers <- U$getFollowerIDs(cainfo=CAInfo)
  # get screen name
  friends   <- lookupUsers(friends, cainfo=CAInfo)
  followers <- lookupUsers(followers, cainfo=CAInfo)
  TargetScreenNames <- sapply(friends,"[[","screenName")
  SourceScreenNames <- sapply(followers,"[[","screenName")
  # unnaming ScreenNames
  names(SourceScreenNames) <- NULL
  names(TargetScreenNames) <- NULL
  # creating edgelist from followers
  FollowEdges <- data.frame(Source=c(rep(User,length(TargetScreenNames)),
                                     SourceScreenNames),
                            Target=c(TargetScreenNames,
                                     rep(User,length(SourceScreenNames)))
                            )
  return(FollowEdges)
  }
```
Now the same for more users:

```{r function to get multiple users' followers}
followerLists <- function(Users=NULL, #string vector of screen_names or IDs
                          CAInfo="doga/crawler/cacert.perm"){ #srting of certification file

# This function creates a data.frame edgelist of the given Twitter Users

  FollowEdges <- adply(Users, 1, followerList, CAInfo=CAInfo)  
  return(FollowEdges[c("Source","Target")])
  }
```

Lets create the crawler!

## VERSION 1.

This wont work because of TwitterAPI limitations 

```{r crawler 1}
usersNetwork <- function(startUsers=NULL, #string vector of screen_names or IDs
                         Nmax=1000000, #maximum number of users in network
                         outfile="doga/crawler/followedgelist.csv", # name of outfile
                         CAInfo="doga/crawler/cacert.perm"){ #certification file

# This function collects at least Nmax users and their
# following-follower relations. The last step is the first
# when the amount of users exceeds the Nmax limit.

  #creating the checklist of users to follow, whose edgelist do we got.
  UList <- data.frame(scr_name=startUsers, chckd=FALSE)
  write.csv(data.frame(Source=NULL,Target=NULL),file=outfile)

  # The crawling
  while(NROW(UList) < Nmax){
    # getting the users still not checked
    scanUsers <- levels(UList[UList$chckd == FALSE,1])
    # get the edgelist of scanUsers
    el <- followerLists(scanUsers,CAInfo=CAInfo)
    # writing the edgelist out
    write.csv(el,file=outfile,append=TRUE)

    # getting the list of users
    UNew <- unique(unlist(el))
    # create new UList with setting chckd to TRUE
    UListNew <- data.frame(scr_name=UNew, chckd=FALSE)
    # the rows which were in the previous list will have chckd = TRUE
    UListNew[UNew %in% levels(UList$scr_name),"chckd"] <- TRUE
    # overwrite old UList
    # clean the house:
    Ulist <- UListNew
    rm(scanUser, el, Unew, UListNew)
    }
  cat(paste(NROW(UList), " nodes collected,\n",
            sum(UList$chckd), " were checked.\n",
            "Full edgelist can be found at ", outfile, " .",
            sep=""))
  return(UList)
  }
```  
*NOTE TO SELF:* hint: getCurRateLimitInfo(cainfo="doga/crawler/cacert.perm")    
 
## VERSION 2.

Let's try with a slow for, check the usercount and the remaining hits (API calls).

getFriends() and getFollowers() works only for the registered user, so I have to use ID-s later. It was a bug because of Twitter API 1.1. , problem solved by downloading new version from Github. For more, scroll down.

```{r crawling with getFriends()}
usersNetwork <- function(startUsers=NULL, #string vector of screen_names or IDs
                         Nmax=1000000, #maximum number of users in network
                         outfile=Outfile, # name of outfile
                         CAInfo=cacert){ #certification file

  if(!require("plyr") |
     !require("ROAuth") |
     !require("twitteR")){
    stop("Please check packages: plyr, ROAuth, twitteR")
    }
  if(!is.numeric(Nmax)){
    stop("Nmax must be an integer!")
    } 

# This function creates an edgelist based on Twitter followers.
# Does that by getting an initial User screen_name vector,
#  collecting their followers and followings, then select the ones who are new,
#   and repeating until it collected Nmax users.
# Because of limitations of API calls, it rests quite often.

  # creating the data file to hard drive:
  write.table(data.frame(Source="Source", Target="Target"),
              file=outfile,
              row.names=FALSE,
              append=FALSE,
              col.names=FALSE,
              sep=",",
              quote=TRUE)
  # creating user checker:
  UserControl <- data.frame(User=startUsers, chkd=FALSE)

  # let's crawl!
  while(NROW(UserControl) < Nmax){
    # getting the amount of necesarry APIcalls
    UserList <- lookupUsers(as.character(UserControl[UserControl$chkd == FALSE, "User"]),cainfo=CAInfo)
    # for each user, if the remaining calls are enough, getting the data of friends and followers (2 calls per user)
    for (i in 1:length(UserList)){
      U <- UserList[[i]]
      R <- getCurRateLimitInfo(cainfo=CAInfo)
      if(R$remainingHits < 2){cat(paste("Waiting till", R$resetTime, ".", sep=" "))
        Sys.sleep(3600)
        }
      # getting the friends' & followers' data
      UFriends   <- U$getFriends(cainfo=CAInfo)
      UFollowers <- U$getFollowers(cainfo=CAInfo)
      # putting a tick in the UserControl
      UserControl[UserControl$User == U$screenName, "chkd"] <- TRUE
      # extracting new users from UFriends and UFollowers
      UFNames <- c(laply(UFriends, "[[", "screenName"), laply(UFollowers, "[[", "screenName"))
      UserControl <- data.frame(User=c(as.character(UserControl$User), UFNames[!(UFNames %in% as.character(UserControl$User))]),
                                chkd =c(UserControl$chkd, rep(FALSE,
                                                               length(UFNames[!(UFNames %in% as.character(UserControl$User))]))
                                        )
                                )
      #append new in- & outdegrees
      EdgeList1 <- data.frame(Source=as.character(U$screenName),
                             Target=as.character(laply(UFriends, "[[", "screenName"))
                             )
      EdgeList2 <- data.frame(Source=as.character(laply(UFollowers, "[[", "screenName")),
                              Target=as.character(U$screenName))
      EdgeList=rbind(EdgeList1, EdgeList2)
      # append the new table to the old one
      write.table(EdgeList,
            file=outfile,
            row.names=FALSE,
            append=TRUE,
            col.names=FALSE,
            sep=",",
            quote=TRUE)
      #refresh UserList:
      
      }
    # if there is no more users, break:
    if(sum(UserControl$chkd == FALSE) == 0){
      break()
      }
    }
  return(UserControl)
  }
```

## VERSION 3.

This time I will try to use IDs instead of screenNames. This version saves the UserIndex to hard drive.

```{r Twitter crawler, saves the UserIndex to the hard drice too}
usersNetwork <- function(startUsers=NULL, #string vector of screen_names or IDs
                         Nmax=length(startUsers), #maximum number of users in network
                         EdgeFile=Outfile1, # name of edgelist
                         IndexFile=Outfile2 # name of the indexfile 
                         CAInfo=cacert){ #certification file

  if(!require("plyr") |
     !require("ROAuth") |
     !require("twitteR")){
    stop("Please check packages: plyr, ROAuth, twitteR")
    }
  if(!is.numeric(Nmax)){
    stop("Nmax must be an integer!")
    } 

# This function creates an edgelist based on Twitter followers.
# Does that by getting an initial User screen_name vector,
#  after that it uses the IDs,
#  collecting their followers and followings, then select the ones who are new,
#  and repeating until it collected Nmax users.
# Because of limitations of API calls, it rests quite often.

  # creating the data file to hard drive:
  EdgeList <- data.frame(x1="Source", x2="Target")
  write.table(EdgeList,
              file=outfile,
              row.names=FALSE,
              append=FALSE,
              col.names=FALSE,
              sep=",",
              quote=TRUE)
  # getting the IDs of startUsers, and creating the checklist:
  startUsers <- lookupUsers(startUsers, cainfo=CAInfo)
  startUsers <- as.character(laply(startUsers,"[[","id"))
  
  # creating the checklist:
  UserControl <- data.frame(User=startUsers, chkd=FALSE)
  # saving it to hard drive:
  
  # let's crawl!
  while(sum(UserControl$chkd == TRUE) < Nmax){
    print(paste(sum(UserControl$chkd == TRUE), " user checked from ", Nmax, sep=""))
    # getting the amount of necesarry APIcalls
    UserList <- lookupUsers(as.character(UserControl[UserControl$chkd == FALSE, "User"]),cainfo=CAInfo)
    # for each user, if the remaining calls are enough, getting the data of friends and followers (2 calls per user)
    for (i in 1:length(UserList)){
      #here's an unelegant break in the case of exceeding Nmax:
      if(sum(UserControl$chkd == TRUE) > Nmax){
        break()
        }
      U <- UserList[[i]]
      print(paste("User: ",U$screenName, sep=""))
      R <- getCurRateLimitInfo(cainfo=CAInfo)
      while(sum(as.numeric(R$remaining) < 2) > 0){
        print(paste("Waiting till ", R$reset[10]+3600, ", ", sum(UserControl$chkd == TRUE), "/", NROW(UserControl), "checked.", sep=""))
        Sys.sleep(60)
        R <- getCurRateLimitInfo(cainfo=CAInfo)
        }
      # getting the friends' & followers' data
      UFriends   <- U$getFriendIDs(blockOnRateLimit=TRUE, cainfo=CAInfo)
      UFollowers <- U$getFollowerIDs(blockOnRateLimit=TRUE, cainfo=CAInfo)
      # putting a tick in the UserControl
      UserControl[UserControl$User == U$id, "chkd"] <- TRUE
      # extracting new users from UFriends and UFollowers
      UFIDs <- unique(c(UFriends, UFollowers))
      UserControl <- data.frame(User=c(as.character(UserControl$User), UFIDs[!(UFIDs %in% as.character(UserControl$User))]),
                                chkd =c(UserControl$chkd, rep(FALSE,
                                                               length(UFIDs[!(UFIDs %in% as.character(UserControl$User))])
                                                              )
                                        )
                                )
      #append new in- & outdegrees
      EdgeList1 <- data.frame(Source=as.character(U$id),
                             Target=UFriends
                             )
      EdgeList2 <- data.frame(Source=UFollowers,
                              Target=as.character(U$id))
      EdgeList=rbind(EdgeList1, EdgeList2)
      write.table(EdgeList,
            file=outfile,
            row.names=FALSE,
            append=TRUE,
            col.names=FALSE,
            sep=",",
            quote=TRUE)
      #clean up the memory:
      rm(EdgeList, EdgeList1, EdgeList2, UFIDs, UFriends, UFollowers, R, U)
      }
    # if there is no more users, break:
    if(sum(UserControl$chkd == FALSE) == 0){
      break()
      }
    }
  return(UserControl)
  }
```
## HA-HA-HA-HA-HA-HA-HA-HA!!!!

Version 2 and 3 should work, 2 is better, but this version has a bug because of changes from Twitter API 1.0 to 1.1. [INFO] [http://stackoverflow.com/questions/13873154/how-to-get-twitter-user-friends-followers-by-twitter-package]

Download from github...
```{r download fresh twitteR}
# library(devtools)
# dev_mode(on=TRUE)
# install_github("twitter", username="geoffjentry")
# dev_mode(on=FALSE)
```
with twitteR 0.99.26 the getFriends throws an error:

`   [1] "Bad Gateway"`

`Error in twInterfaceObj$doAPICall(paste("users", "lookup", sep = "/"),  :`

`   Error: Bad Gateway`
 
 That's why I will use VERSION 3. which collects IDs.

## VERSION 4.

This version works entirely on hard drive. It stores not only the edgelist there, but also the index of users. First it checks if any users were checked if a filename is given. If NULL, gives a warning. Index is returned. Also the name is changed to CreateEdgeList 

```{r final version, hopefully}
CreateEdgeList <- function(start.users,  # character vector of screenNames is IDs
                           index.csv,  # csv about users
                           edge.list.csv,  # csv where edgelist should be stored 
                           cainfo,  # where is SSL cerification file
                           number.to.check=length(start.users)  # to definiate end

  ##################               #######################
  # TODO: handling errors in index.csv and edge.list.csv #
  #       solved by file.exists()                        #
  ##################               #######################

  # Checking inputs
  # start.users
  if(!is.character(start.users) || !is.null(start.users)){
    stop("start.users should be as character, or NULL!")
    }
  # index.csv
  if(!is.character(index.csv) || !is.null(index.csv)){
    stop("index.csv should be as character, or NULL!")
    }
  # edgel.list.csv
  if(!is.character(edge.list.csv))){
    stop("edge.list.csv should be as character!")
    }
  # cainfo
  if(!is.character(cainfo)){
    stop("cainfo should be as character!")
    }
  # number.to.check
  if(!is.numeric(B) || (number.to.check %% 1) != 0){
    stop("number.to.check should be integer!")
    }

  # Checking index.csv and edge.list.csv
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
      } else{
      append.index <- TRUE
      }
    write.index <- TRUE
    }
    if(file.exists(edge.list.csv)){
      write.table(data.frame(source="source", target="target"),
              file=edge.list.csv,
              row.names=FALSE,
              append=FALSE,
              col.names=FALSE,
              sep=",",
              quote=TRUE)
      warning("No index file found, new edge list created.",
              call.=FALSE)
      } else{
      append.edge.list <- TRUE
      }

  # Initiate the index:
  start.users.id <- lookupUsers(start.users, cainfo=cainfo)
  start.users.id <- laply(start.users.id, "[[", "id")
  if(append.index){
    user.index <- read.csv(index.csv)
    user.index <- rbind(user.index,data.frame(user=start.users.id[
                    !(start.users.id %in% as.character(user.index$user))
                    ],
                                              chkd=FALSE))
    } else{
    user.index <- data.frame(user=start.users.id, chkd=FALSE)
  # clean memory
  rm(start.users.id, start.users)

  # raise Nmax with the number of users already checked
  Nmax <- Nmax + sum(user.index$chkd)

  # I cant use llply to methods ($getFriends),
  # so I will have to check one by one.
  # I could get more User,
  # but there is a smaller limit for friends and followers.
  while(sum(user.index$chkd) < Nmax){
    user <- getUser(users.index$user[min(which(user.index$chkd == FALSE))],
                    cainfo=cainfo)
    user.friend <- user$getFriendsIds(cainfo=cainfo)
    user.follower <- user$getFollowerIDs(cainfo=cainfo)
    
    }
  
  }
```  
  
Now get the tweets of users, and get hashtags and mentions from text (additionally: if reply - replyToUid, replyToSn, if retweet - statusSource)
