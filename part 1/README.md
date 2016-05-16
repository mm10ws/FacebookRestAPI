Read me:

Group Members:
Mayur Mehta   
Pallavi Joshi 

Important Files in the submission:
build.sbt- the sbt build file

server.scala- Server is the facebook server which implements the REST API.
master.scala- Master starts clients
client.scala- This contains the user behavior simulation and the parameters for each user.

Usage:
sbt "run <numofclients> <number of tasks per client>"

Example usage:
sbt "run 1000 5"

Description :

Facebook is simulated by using one server and multiple clients. The design currently supports upto 1000 clients efficiently.
It might take a little longer to start the simulation for a larger no. of clients as it builds its initial values.
Before starting the simulation, we initialize all  the clients with:
1.a page with no posts but having the characteristics of a profile(birthday,name,personal-info etc)
2.an empty album
3. a friendlist

During simulation, the clients make requests at periodic intervals of 5000 microseconds.
This is to show that the clients could not possibly be making requests all the time which is more realistic.
The clients randomly choose tasks
However, posting a photo or an update 
1. post on a friends page/your own page (sendPost),comments are randomly generated
2. looking at a page (getPage)
3. posting photos to a page and uploading photos to an album(sendPhoto)
4. looking at photos on a page (getPhotoFromPage)	
5. like pages(sendLike)
6. looking at posts on pages(getPostFromPage)
7. looking at photos in an album (getPhotoFromAlbum)

We've also measured the simulation time in milliseconds,starting from the time we started the simulation till all tasks are complete.


Statistics :

Every 60 seconds on Facebook: 510 comments are posted, 293,000 statuses are updated, and 136,000 photos are uploaded. (Source: The Social Skinny) 
Source:
https://zephoria.com/top-15-valuable-facebook-statistics/


As per this link, sharing/posting photos is the most likely activity, sending posts/updates,commenting on a post, clicking the facebook like button
Source:
http://www.jeffbullas.com/2013/09/20/12-awesome-social-media-facts-and-statistics-for-2013/


