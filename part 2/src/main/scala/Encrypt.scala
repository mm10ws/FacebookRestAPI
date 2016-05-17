import java.io.ByteArrayInputStream
import java.security.spec._
import java.security.{SecureRandom, _}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import scala.collection.mutable.ArrayBuffer

object Encrypt {
  //var testkey = ""

  //add try catch
  def AESEncrypt(plaintext: String) = {
    val rand = new SecureRandom()
    val encoder = Base64.getEncoder
    val iv = new Array[Byte](16)
    val key = new Array[Byte](16)
    rand.nextBytes(iv)
    rand.nextBytes(key)
    val testkey = encoder.encodeToString(key)
    //println(testkey)

    val secretKey = new SecretKeySpec(key, "AES")
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding") //initialize the cipher
    val plaintextBytes = plaintext.getBytes
    cipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(iv))
    val encryptedBytes = cipher.doFinal(plaintextBytes) //encrypt
    val message = new ArrayBuffer[Byte]
    //append iv to ciphertext
    message ++= iv
    message ++= encryptedBytes
    val cipherText = encoder.encodeToString(message.toArray) //convert
    (testkey, cipherText)
  }

  def AESDecrypt(ciphertext: String, key: String): String = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding") //initialize the cipher
    val decoder = Base64.getDecoder
    val decoded = decoder.decode(ciphertext)
    val keybytes = decoder.decode(key)
    val secretKey = new SecretKeySpec(keybytes, "AES")
    val iv = decoded.slice(0, 16) //get the iv
    cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(iv))
    val plaintext = cipher.doFinal(decoded.slice(16, decoded.length))
    new String(plaintext)
  }

  def main(args: Array[String]) = {
    val e = Encrypt
    val ctext = e.AESEncrypt("this is a new looooooooooooooooooooooooooooooooooooooooooooooooooooooooog {secret} ? [] <> /")
    //println(ctext)
    println(AESDecrypt(ctext._2, ctext._1))

  }
}

object PublicKeyEncrypt {

  def generateKeyPair(keyType: String) = {
    val keyGen = KeyPairGenerator.getInstance(keyType)
    val random = SecureRandom.getInstance("SHA1PRNG", "SUN")
    keyGen.initialize(2048, random)
    val kpair = keyGen.generateKeyPair()
    keyPairString(kpair)
  }

  def keyPairString(keyPair: KeyPair) = {
    val pubKey = keyPair.getPublic
    if (pubKey.getFormat != "X.509")
      throw new RuntimeException("Can't produce public key in format: " + pubKey.getFormat)

    val privKey = keyPair.getPrivate
    if (privKey.getFormat != "PKCS#8")
      throw new RuntimeException("Can't produce private key in format: " + privKey.getFormat)

    val encoder = Base64.getEncoder
    (encoder.encodeToString(pubKey.getEncoded), encoder.encodeToString(privKey.getEncoded))
  }

  def readPublicKey(keyType: String, publicKey: Array[Byte]) = {
    //makes a publicKey object from public key bytes
    val pubKeySpec = new X509EncodedKeySpec(publicKey)
    KeyFactory.getInstance(keyType).generatePublic(pubKeySpec)
  }

  def readPrivateKey(keyType: String, privateKey: Array[Byte]) = {
    //makes a privateKey object from private key bytes
    val privKeySpec = new PKCS8EncodedKeySpec(privateKey)
    KeyFactory.getInstance(keyType).generatePrivate(privKeySpec)
  }

  def readKeyPair(keyType: String, publicKey: Array[Byte], privateKey: Array[Byte]) = {
    new KeyPair(readPublicKey(keyType, publicKey), readPrivateKey(keyType, privateKey))
  }

  def sign(source: String, key: String): Array[Byte] = {
    val decoder = Base64.getDecoder
    val instream = new ByteArrayInputStream(source.getBytes)
    val dsa = Signature.getInstance("SHA1withDSA")
    dsa.initSign(readPrivateKey("DSA", decoder.decode(key)))
    val inBytes = new Array[Byte](1024)
    var count = instream.read(inBytes)
    while (count >= 0) {
      dsa.update(inBytes, 0, count)
      count = instream.read(inBytes)
    }
    dsa.sign()
  }

  def verify(source: String, key: String, sig: Array[Byte]): Boolean = {
    val decoder = Base64.getDecoder
    val instream = new ByteArrayInputStream(source.getBytes)
    val dsa = Signature.getInstance("SHA1withDSA")
    dsa.initVerify(readPublicKey("DSA", decoder.decode(key)))
    val inBytes = new Array[Byte](1024)
    var count = instream.read(inBytes)
    while (count >= 0) {
      dsa.update(inBytes, 0, count)
      count = instream.read(inBytes)
    }
    dsa.verify(sig)
  }

  def encrypt(source: String, key: String): String = {
    val encoder = Base64.getEncoder
    val decoder = Base64.getDecoder
    val k = readPublicKey("RSA", decoder.decode(key))
    val cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, k)
    val arr = cipher.doFinal(source.getBytes)
    val cipherText = encoder.encodeToString(arr)
    cipherText
  }

  def decrypt(source: String, key: String): String = {
    val decoder = Base64.getDecoder
    val decoded = decoder.decode(source)
    val k = readPrivateKey("RSA", decoder.decode(key))
    val cipher = Cipher.getInstance("RSA") //initialize the cipher
    cipher.init(Cipher.DECRYPT_MODE, k)
    val plaintext = cipher.doFinal(decoded)
    new String(plaintext)
  }

  def main(args: Array[String]) = {
    val e = PublicKeyEncrypt
    val keypair = e.generateKeyPair("RSA") //change to RSA to test encrypt/decrypt or to DSA to test digital signatures
    val public_key = keypair._1
    val private_key = keypair._2

    val testString = "hello this is a test of RSA with base 64 decoding and encoding loooooooooooooooooooooooooooooooooooooooong"

        val ctext = e.encrypt(testString, public_key)
        println(ctext)

        val decrypted = e.decrypt(ctext, private_key)

        println(decrypted)

//    val signed = e.sign(testString, private_key)
//    println(e.verify(testString, public_key, signed))

  }
}