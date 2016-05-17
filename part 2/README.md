Read me:Project4 -Part2

Group Members:
Mayur Mehta   
Pallavi Joshi 

Important Files in the submission:
build.sbt- the sbt build file
Encrypt.scala- includes all the security prototypes--DSA,RSA and AES.
server.scala- Server is the facebook server which implements the REST API.
master.scala- Master starts clients
client.scala- This contains the user behavior simulation and the parameters for each user.

Usage:
sbt "run <numofclients> <number of tasks per client>"

Example usage:
sbt "run 1000 5"

Description :

Secure Facebook is simulated by using one server and multiple clients. The design without security and authentication supported upto 1000 clients efficiently.However, with the current authentication amd security that we have implemented it supports 300 clients properly.

Design Flow:
 Initially, we generate RSA and DSA public and private key pairs for all clients. 
The private keys are held only with the client and are never shared.
The public keys are sent to the server.These public keys are stored in the clear on the server.So, the public keys  for all clients could be obtained by the clients from the server. 

We first authenticate all the clients as they make an authentication /establishSession request. The authentication is carried out as per FIPS 196 unilateral protocol.
Client sends authentication request to the server, the server replies back with a random no challenge for the client. The client digitally signs the random bit with its private key. The server decrypts this with its public key and verifies the signature. If true, the client is authenticated and assigned a tokenid which the server sends back in a reply to the client and also stores the token in its tokenMap for future authentication.Once assigned a token , the client must send its token with every request for the server to verify its identity using the token map.

All the data to be stored on the server is encrypted.The server can read only the meta-data-friendslist,likes,mapping for post to page. All the other data is encrypted using AES-128 and CBC mode since RSA -2048 did not support to encrpyt a string more than 245 bytes. Since, symmetric  hashing is itself not so secure, we encrypt the AES key with RSA-2048.

AES generates new secret key  and Iv every time it is called using secure random number generation.

After the initialization of clients data like friendlist, page which we enclose in futures, we start the simulation by calling various activities like sending posts to friends page,get page,get posts from page and liking pages  at periodic intervals of 5000 microseconds.
   
In order to read from a friends page or post to a friends page we must encrypt the data using the friend's RSA public key so that the friend can view the posts by decrypting with its private key.

We've also measured the simulation time in milliseconds,starting from the time we started the simulation/started activities till all tasks are complete.




