//********** COPYRIGHT 2013 altek Corporation *********************************
// FILE NAME:   This is template file.
// VERSION:     $Revision: 19842 $
// DESCRIPTION: The file is for java framework service jni
// 
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#define LOG_NDEBUG 0 //This macro is used to switch show/disable logcat
#define LOG_TAG "KeyexchangeJNI"

#include "jni.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <jpake.h>
#include <hash_drbg.h>

#include <android/log.h>



#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )


#define JPAK_ID_LNG 4
#define JPAK_FE_LNG 20
#define JPAK_STEP1_MSG_LNG 284
#define JPAK_STEP2_MSG_LNG 144
#define JPAK_STEP3_MSG_LNG 32
#define JPAK_RANDOM_MSG_LNG 256
#define JPAK_SESSION_KEY_LNG 32

//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/ble/blemanager/KeyExchagneJNI";
     
/*
 * The method below are not thread-safe and not intended to be 
 */
#ifdef CALLBACK
struct fields_t {
    jclass    clazzEffect;
    jmethodID midPostNativeEvent;    
}; 
static fields_t fields;
#endif //CALLBACK

EscHashDrbg_ContextT ctxRB;
EscJPake_ContextT ctxB;
UINT8 idB[ID_LNG];
UINT8 randomX3[FE_LNG];
UINT8 randomV3[FE_LNG];
UINT8 randomX4[FE_LNG];
UINT8 randomV4[FE_LNG];

UINT8 M2[STEP1_MSG_LNG];
UINT8 M4[STEP2_MSG_LNG];
UINT8 M6[JPAK_STEP3_MSG_LNG];
UINT8 randomA[FE_LNG];
UINT8 randomB[FE_LNG];
/* established session key */
UINT8 keyA[SESSION_KEY_LNG];
UINT8 keyB[SESSION_KEY_LNG];

 UINT8 secretWrong[FE_LNG];

//const UINT8 entropy[32] =
//{
//	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
//	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
//	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
//	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
//};
//const UINT8 nonce[16] =
//{
//	0x00,0x01,0x02,0x03,0x04,0x05, 0x06,0x07,
//	0x08,0x09,0x0A,0x0B,0x0C,0x0D, 0x0E,0x0F
//};
//const UINT8 secret[FE_LNG] = // 20 bytes input pin
//{
//	0xD3, 0x5D, 0xB7, 0xE3, 0x9E, 0xBB, 0x00, 0x20, 0x94, 0xF8,
//	0x21, 0xC4, 0x2B, 0xF2, 0x4F, 0x00, 0xA1, 0x3F, 0x28, 0x46
//};

static void
__Kes_test(JNIEnv *env, jclass clazz)
{
	ALOGD("[%s] ++\n", __FUNCTION__);
//	   const UINT8 entropy[32] =
//	   {
//			   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
//			   0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
//			   0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
//			   0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
//	   };
//	   const UINT8 nonce[16] =
//	   {
//			   0x00,0x01,0x02,0x03,0x04,0x05, 0x06,0x07,
//			   0x08,0x09,0x0A,0x0B,0x0C,0x0D, 0x0E,0x0F
//	   };
//       const UINT8 secret[JPAK_FE_LNG] =
//	   {
//	           0xD3, 0x5D, 0xB7, 0xE3, 0x9E, 0xBB, 0x00, 0x20, 0x94, 0xF8,
//	           0x21, 0xC4, 0x2B, 0xF2, 0x4F, 0x00, 0xA1, 0x3F, 0x28, 0x46
//	   };
//       //	   UINT8 secret[FE_LNG];
//       UINT8 M1[JPAK_STEP1_MSG_LNG];
//	   UINT8 M2[JPAK_STEP1_MSG_LNG];
//	   UINT8 M3[JPAK_STEP2_MSG_LNG];
//	   UINT8 M4[JPAK_STEP2_MSG_LNG];
//	   UINT8 M5[JPAK_STEP3_MSG_LNG];
//	   UINT8 M6[JPAK_STEP3_MSG_LNG];
//	   UINT8 randomA[JPAK_RANDOM_MSG_LNG];
//	   UINT8 randomB[JPAK_RANDOM_MSG_LNG];
//	   /* established session key */
//	   UINT8 keyA[JPAK_SESSION_KEY_LNG];
//	   UINT8 keyB[JPAK_SESSION_KEY_LNG];
//
//
//	   UINT8 secretWrong[JPAK_FE_LNG];
//	   //**************************
//	   EscHashDrbg_ContextT ctxRA;
//	   EscJPake_ContextT ctxA;
//	   UINT8 idA[JPAK_ID_LNG];
//	   UINT8 randomX1[JPAK_FE_LNG];
//	   UINT8 randomV1[2*JPAK_FE_LNG];
//	   UINT8 randomX2[JPAK_FE_LNG];
//	   UINT8 randomV2[2*JPAK_FE_LNG];
//	   EscHashDrbg_Init(&ctxRA,entropy,nonce);
//	   EscHashDrbg_GetRandom(&ctxRA,idA);
//	   ALOGD("A------0=[%d], 1=[%d], 2=[%d], 3=[%d]", idA[0], idA[1], idA[2], idA[3]);
//	   EscHashDrbg_GetRandom(&ctxRA,randomX1);
//	   EscHashDrbg_GetRandom(&ctxRA,randomX2);
//	   EscHashDrbg_GetRandom(&ctxRA,randomV1);
//	   EscHashDrbg_GetRandom(&ctxRA,randomV2);
//	   EscJPake_init(&ctxA, idA);
//	   EscJPake_generateStep1Message(&ctxA, randomX1, randomV1, randomX2, randomV2, M1);
//	   //*****************************
//
//	   //**************************
//	   EscHashDrbg_ContextT ctxRB;
//	   EscJPake_ContextT ctxB;
//	   UINT8 idB[JPAK_ID_LNG];
//	   UINT8 randomX3[JPAK_FE_LNG];
//	   UINT8 randomV3[2*JPAK_FE_LNG];
//	   UINT8 randomX4[JPAK_FE_LNG];
//	   UINT8 randomV4[2*JPAK_FE_LNG];
//	   EscHashDrbg_Init(&ctxRB,entropy,nonce);
//
//	   EscHashDrbg_GetRandom(&ctxRB,idB);
//	   ALOGD("B-----0=[%d], 1=[%d], 2=[%d], 3=[%d]", idB[0], idB[1], idB[2], idB[3]);
//	   EscHashDrbg_GetRandom(&ctxRB,randomX3);
//	   EscHashDrbg_GetRandom(&ctxRB,randomX4);
//	   EscHashDrbg_GetRandom(&ctxRB,randomV3);
//	   EscHashDrbg_GetRandom(&ctxRB,randomV4);
//	   EscJPake_init(&ctxB, idB);
//	   //EscJPake_generateStep1Message(&ctxB, randomX3, randomV3, randomX4, randomV4, M2);
//	   //*****************************
//	   ALOGD("[%s] --STEP 1--\n", __FUNCTION__);
//	   if(EscJPake_verifyStep1Message(&ctxB, M1) == EscJPakeErr_NO_ERROR)
//	   {
//		   ALOGD("[%s] --M1 OK--\n", __FUNCTION__);
//		   EscJPake_generateStep1Message(&ctxB, randomX3, randomV3, randomX4, randomV4, M2);
//	   }
//
//
//	   if(EscJPake_verifyStep1Message(&ctxA, M2) == EscJPakeErr_NO_ERROR)
//	   {
//		   ALOGD("[%s] --M2 OK--\n", __FUNCTION__);
//		   ALOGD("JPAK_ID_LNG-----=[%d]",JPAK_ID_LNG);
//		   ALOGD("JPAK_FE_LNG-----=[%d]",JPAK_FE_LNG);
//		   ALOGD("JPAK_STEP1_MSG_LNG-----=[%d]",JPAK_STEP1_MSG_LNG);
//		   ALOGD("JPAK_STEP2_MSG_LNG-----=[%d]",JPAK_STEP2_MSG_LNG);
//		   ALOGD("JPAK_STEP3_MSG_LNG-----=[%d]",JPAK_STEP3_MSG_LNG);
//		   EscJPake_generateStep2Message(&ctxA, randomA, secret, M3);
//	   }
//	   ALOGD("[%s] --STEP 2--\n", __FUNCTION__);
//	   if(EscJPake_verifyStep2Message(&ctxB, M3) == EscJPakeErr_NO_ERROR)
//	   {
//	   		ALOGD("[%s] --M3 OK--\n", __FUNCTION__);
//	   		EscJPake_generateStep2Message(&ctxB, randomB, secret, M4);
//	   		EscJPake_computeKey(&ctxB, keyB);
//	   }
//	   if(EscJPake_verifyStep2Message(&ctxA, M4) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M4 OK--\n", __FUNCTION__);
//	   	    EscJPake_computeKey(&ctxA, keyA);
//	   	    EscJPake_generateConfirmationM5(&ctxA, M5);
//	   }
//	   ALOGD("[%s] --STEP 3--\n", __FUNCTION__);
//	   if(EscJPake_verifyConfirmationM5(&ctxB, M5) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M5 OK--\n", __FUNCTION__);
//	   	    EscJPake_generateConfirmationM6(&ctxB, M6);
//	   }
//	   if(EscJPake_verifyConfirmationM6(&ctxA, M6) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M6 OK--\n", __FUNCTION__);
//
//	   }
	   ALOGD("[%s] --END--\n", __FUNCTION__);
	ALOGD("[%s] --\n", __FUNCTION__);
}

//*****************************************************************************
// FUNCTION NAME: __Kes_init
// DESCRIPTION:   KES initialization
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       NA
//*****************************************************************************
static void
__Kes_init(JNIEnv *env, jclass clazz, jbyteArray entropy, jbyteArray nonce)
{
	int i=0;
	ALOGD("[%s] ++\n", __FUNCTION__);
//	const UINT8 entropy[32] =
//	{
//		0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
//		0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
//		0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
//		0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
//    };
//	const UINT8 nonce[16] =
//	{
//		 0x00,0x01,0x02,0x03,0x04,0x05, 0x06,0x07,
//		 0x08,0x09,0x0A,0x0B,0x0C,0x0D, 0x0E,0x0F
//	};
//	const UINT8 secret[FE_LNG] =
//	{
//		 0xD3, 0x5D, 0xB7, 0xE3, 0x9E, 0xBB, 0x00, 0x20, 0x94, 0xF8,
//		 0x21, 0xC4, 0x2B, 0xF2, 0x4F, 0x00, 0xA1, 0x3F, 0x28, 0x46
//	};
//	   const UINT8 entropy[32] =
//	   {
//			   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
//			   0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
//			   0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
//			   0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
//	   };
//	   const UINT8 nonce[16] =
//	   {
//			   0x00,0x01,0x02,0x03,0x04,0x05, 0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D, 0x0E,0x0F
//	   };
//	   //Pin Code 20 byte
//       const UINT8 secret[FE_LNG] =
//	   {
//	    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
//		0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
//	    0x20, 0x21, 0x22, 0x23
//	   };
       //	   UINT8 secret[FE_LNG];
//       UINT8 M1[STEP1_MSG_LNG];
//	   UINT8 M2[STEP1_MSG_LNG];
//	   UINT8 M3[STEP2_MSG_LNG];
//	   UINT8 M4[STEP2_MSG_LNG];
//	   UINT8 M5[EscSha256_DIGEST_LEN];
//	   UINT8 M6[EscSha256_DIGEST_LEN];
//	   UINT8 randomA[FE_LNG];
//	   UINT8 randomB[FE_LNG];
//	   /* established session key */
//	   UINT8 keyA[SESSION_KEY_LNG];
//	   UINT8 keyB[SESSION_KEY_LNG];
//
//
//	   UINT8 secretWrong[FE_LNG];
	   //**************************
//	   EscHashDrbg_ContextT ctxRA;
//	   EscJPake_ContextT ctxA;
//	   UINT8 idA[ID_LNG];
//	   UINT8 randomX1[FE_LNG];
//	   UINT8 randomV1[2*FE_LNG];
//	   UINT8 randomX2[FE_LNG];
//	   UINT8 randomV2[2*FE_LNG];
//	   EscHashDrbg_Init(&ctxRA,entropy,nonce);
//	   EscHashDrbg_GetRandom(&ctxRA,idA);
//	   ALOGD("A------0=[%d], 1=[%d], 2=[%d], 3=[%d]", idA[0], idA[1], idA[2], idA[3]);
//	   EscHashDrbg_GetRandom(&ctxRA,randomX1);
//	   EscHashDrbg_GetRandom(&ctxRA,randomX1);
//	   EscHashDrbg_GetRandom(&ctxRA,randomV2);
//	   EscHashDrbg_GetRandom(&ctxRA,randomV2);
//	   EscJPake_init(&ctxA, idA);
//	   EscJPake_generateStep1Message(&ctxA, randomX1, randomV1, randomX2, randomV2, M1);
	   //*****************************

	   //**************************
//	   EscHashDrbg_ContextT ctxRB;
//	   EscJPake_ContextT ctxB;
//	   UINT8 idB[ID_LNG];
//	   UINT8 randomX3[FE_LNG];
//	   UINT8 randomV3[FE_LNG];
//	   UINT8 randomX4[FE_LNG];
//	   UINT8 randomV4[FE_LNG];

	   jbyte* pEntropy = env -> GetByteArrayElements(entropy, NULL);
	   jbyte* pNonce= env -> GetByteArrayElements(nonce, NULL);

	   EscHashDrbg_Init(&ctxRB, (UINT8*)pEntropy, (UINT8*)pNonce);

	   EscHashDrbg_GetRandom(&ctxRB,idB);
	   ALOGD("B-----idB----Start--------");
	   for(i=0;i<ID_LNG;i++)
	   {
		   ALOGD("B-----idB[%d]=[%d]", i, idB[i]);
	   }
//	   ALOGD("B-----0=[%d], 1=[%d], 2=[%d], 3=[%d]", idB[0], idB[1], idB[2], idB[3]);
	   EscHashDrbg_GetRandom(&ctxRB,randomX3);
	   ALOGD("randomX3-----randomX3----Start--------");
	   for(i=0;i<FE_LNG;i++)
	   {
		   ALOGD("randomX3-----randomX3[%d]=[%d]", i, randomX3[i]);
	   }
	   EscHashDrbg_GetRandom(&ctxRB,randomX4);
	   ALOGD("randomX4-----randomX4----Start--------");
	   for(i=0;i<FE_LNG;i++)
	   {
		   ALOGD("randomX4-----randomX4[%d]=[%d]", i, randomX4[i]);
	   }
	   EscHashDrbg_GetRandom(&ctxRB,randomV3);
	   for(i=0;i<FE_LNG;i++)
	   {
		   ALOGD("randomV3-----randomV3[%d]=[%d]", i, randomV3[i]);
	   }
	   EscHashDrbg_GetRandom(&ctxRB,randomV4);
	   ALOGD("randomV4-----randomV4----Start--------");
	   for(i=0;i<FE_LNG;i++)
	   {
		   ALOGD("randomV4-----randomV4[%d]=[%d]", i, randomV4[i]);
	   }
	   EscHashDrbg_GetRandom(&ctxRB,randomB);
	   ALOGD("randomB-----randomB----Start--------");
	   for(i=0;i<FE_LNG;i++)
	   {
		   ALOGD("randomB-----randomB[%d]=[%d]", i, randomB[i]);
	   }
	   EscJPake_init(&ctxB, idB);
	   EscJPake_generateStep1Message(&ctxB, randomX3, randomV3, randomX4, randomV4, M2);

	   //*****************************
//	   ALOGD("[%s] --STEP 1--\n", __FUNCTION__);
//	   if(EscJPake_verifyStep1Message(&ctxB, M1) == EscJPakeErr_NO_ERROR)
//	   {
//		   ALOGD("[%s] --M1 OK--\n", __FUNCTION__);
//		   EscJPake_generateStep1Message(&ctxB, randomX3, randomV3, randomX4, randomV4, M2);
//	   }
//
//
//	   if(EscJPake_verifyStep1Message(&ctxA, M2) == EscJPakeErr_NO_ERROR)
//	   {
//		   ALOGD("[%s] --M2 OK--\n", __FUNCTION__);
//		   ALOGD("ID_LNG-----=[%d]",ID_LNG);
//		   ALOGD("FE_LNG-----=[%d]",FE_LNG);
//		   ALOGD("STEP1_MSG_LNG-----=[%d]",STEP1_MSG_LNG);
//		   ALOGD("STEP2_MSG_LNG-----=[%d]",STEP2_MSG_LNG);
//		   ALOGD("EscSha256_DIGEST_LEN-----=[%d]",EscSha256_DIGEST_LEN);
//		   EscJPake_generateStep2Message(&ctxA, randomA, secret, M3);
//	   }
//	   ALOGD("[%s] --STEP 2--\n", __FUNCTION__);
//	   if(EscJPake_verifyStep2Message(&ctxB, M3) == EscJPakeErr_NO_ERROR)
//	   {
//	   		ALOGD("[%s] --M3 OK--\n", __FUNCTION__);
//	   		EscJPake_generateStep2Message(&ctxB, randomB, secret, M4);
//	   		EscJPake_computeKey(&ctxB, keyB);
//	   }
//	   if(EscJPake_verifyStep2Message(&ctxA, M4) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M4 OK--\n", __FUNCTION__);
//	   	    EscJPake_computeKey(&ctxA, keyA);
//	   	    EscJPake_generateConfirmationM5(&ctxA, M5);
//	   }
//	   ALOGD("[%s] --STEP 3--\n", __FUNCTION__);
//	   if(EscJPake_verifyConfirmationM5(&ctxB, M5) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M5 OK--\n", __FUNCTION__);
//	   	    EscJPake_generateConfirmationM6(&ctxB, M6);
//	   }
//	   if(EscJPake_verifyConfirmationM6(&ctxA, M6) == EscJPakeErr_NO_ERROR)
//	   {
//	   	   	ALOGD("[%s] --M6 OK--\n", __FUNCTION__);
//
//	   }
//	   ALOGD("[%s] --END--\n", __FUNCTION__);
	ALOGD("[%s] --\n", __FUNCTION__);
}

//*****************************************************************************
// FUNCTION NAME: __Kes_step1
// DESCRIPTION:   KES Step1 verifies M1 and generate M2
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       byte[]
//*****************************************************************************
static jbyteArray
__Kes_step1(JNIEnv *env, jclass clazz, jbyteArray m1)
{
	ALOGD("[%s] ++\n", __FUNCTION__);
    jbyteArray arr = env -> NewByteArray(JPAK_STEP1_MSG_LNG);
    jbyte* pInput = env -> GetByteArrayElements(m1, NULL);
    jbyteArray result = env->NewByteArray(JPAK_STEP1_MSG_LNG);

    if(pInput != NULL)
    {
//    	ALOGD("[%s] --M1 OK--\n0=[%d], 1=[%d], 2=[%d], 3=[%d]", __FUNCTION__, pInput[0], pInput[1], pInput[2], pInput[3]);

    	if(EscJPake_verifyStep1Message(&ctxB, (UINT8*)pInput) == EscJPakeErr_NO_ERROR)
    	{
    		ALOGD("[%s] --M1 OK--\n", __FUNCTION__);
    		env->SetByteArrayRegion(result, 0, JPAK_STEP1_MSG_LNG, reinterpret_cast<jbyte*>(M2));
    	}
    	else
    	{
    	   ALOGD("[%s] --M1 Failed--\n", __FUNCTION__);
    	}
//    	ALOGD("pInput-----0=[%d], 1=[%d], 2=[%d], 3=[%d]", pInput[0], pInput[1], pInput[2], pInput[3]);
//    	ALOGD("pInput-----16=[%d], 17=[%d], 18=[%d], 19=[%d]", pInput[16], pInput[17], pInput[18], pInput[19]);
    	env->ReleaseByteArrayElements(m1, pInput, JNI_ABORT);
    }
	ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}

//*****************************************************************************
// FUNCTION NAME: __Kes_step2
// DESCRIPTION:   KES Step2 verifies M3, generates M4
//                and generates key
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:      byte[]
//*****************************************************************************
static jbyteArray
__Kes_step2(JNIEnv *env, jclass clazz, jbyteArray m3, jbyteArray secret)
{
	ALOGD("[%s] ++\n", __FUNCTION__);
	jbyteArray arr = env -> NewByteArray(JPAK_STEP2_MSG_LNG);
	jbyte* pInput = env -> GetByteArrayElements(m3, NULL);
    jbyte* pSecret= env -> GetByteArrayElements(secret, NULL);
	jbyteArray result = env->NewByteArray(JPAK_STEP2_MSG_LNG);

	if(pInput != NULL)
	{
		if(EscJPake_verifyStep2Message(&ctxB, (UINT8*)pInput) == EscJPakeErr_NO_ERROR)
		{
		   ALOGD("[%s] --M3 OK--\n", __FUNCTION__);
		   EscJPake_generateStep2Message(&ctxB, randomB, (UINT8*)pSecret, M4);
		   EscJPake_computeKey(&ctxB, keyB);
		   env->SetByteArrayRegion(result, 0, JPAK_STEP2_MSG_LNG, reinterpret_cast<jbyte*>(M4));
//		   env->SetByteArrayRegion(result, 0, 284, reinterpret_cast<jbyte*>(M2));
		}
		else
		{
		   ALOGD("[%s] --M3 Failed--\n", __FUNCTION__);
		}

//		env->ReleaseByteArrayElements(m3, pInput, JNI_ABORT);
	}

	ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}

//*****************************************************************************
// FUNCTION NAME: __Kes_step3
// DESCRIPTION:   KES Step2 verifies M5 and generates M5
//
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       byte[]
//*****************************************************************************
static jbyteArray
__Kes_step3(JNIEnv *env, jclass clazz, jbyteArray m5)
{
	ALOGD("[%s] ++\n", __FUNCTION__);
	jbyteArray arr = env -> NewByteArray(JPAK_STEP3_MSG_LNG);
	jbyte* pInput = env -> GetByteArrayElements(m5, NULL);
	jbyteArray result = env->NewByteArray(JPAK_STEP3_MSG_LNG);

	if(pInput != NULL)
	{
		if(EscJPake_verifyConfirmationM5(&ctxB, (UINT8*)pInput) == EscJPakeErr_NO_ERROR)
		{
		   ALOGD("[%s] --M5 OK--\n", __FUNCTION__);
		   EscJPake_generateConfirmationM6(&ctxB, M6);
		   //EscJPake_computeKey(&ctxB, keyB);
		   env->SetByteArrayRegion(result, 0, JPAK_STEP3_MSG_LNG, reinterpret_cast<jbyte*>(M6));
//		   env->SetByteArrayRegion(result, 0, 284, reinterpret_cast<jbyte*>(M2));
		}
		else
		{
		   ALOGD("[%s] --M5 Failed--\n", __FUNCTION__);
		}
//	    env->ReleaseByteArrayElements(m5, pInput, JNI_ABORT);
	}

	ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}
//*****************************************************************************
// FUNCTION NAME: __Kes_step2
// DESCRIPTION:   KES Step2 verifies M3, generates M4
//                and generates key
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:      byte[]
//*****************************************************************************
static jbyteArray
__Kes_done(JNIEnv *env, jclass clazz)
{
	ALOGD("[%s] ++\n", __FUNCTION__);
	jbyteArray result = env->NewByteArray(JPAK_STEP3_MSG_LNG);

	env->SetByteArrayRegion(result, 0, JPAK_STEP3_MSG_LNG, reinterpret_cast<jbyte*>(keyB));
	ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"Kes_test",     "()V", (void*) __Kes_test },
    {"Kes_init",     "([B[B)V", (void*) __Kes_init },
    {"Kes_step1",     "([B)[B", (void*) __Kes_step1 },
    {"Kes_step2",     "([B[B)[B", (void*) __Kes_step2 },
    {"Kes_step3",     "([B)[B", (void*) __Kes_step3 },
    {"Kes_done",     "()[B", (void*) __Kes_done },
};


//*****************************************************************************
// FUNCTION NAME: registerMethods
// DESCRIPTION:   Register JNINativeMethod function table to DVM.
//
// ARGs:          JNIEnv: jni environment pointer
//
// RETURNS:       int : 
//                  success ==> 0
//                  fail ==> -1
//***************************************************************************** 
int register_custframeworkservice_KeyExchange(JNIEnv* env)
{

    jclass clazz;
    
    /* look up the class */
    clazz = env->FindClass(kClassName);
    if (clazz == NULL) {
        ALOGE("Can't find class %s\n", kClassName);
        return -1;
    }
    
    /* register all the methods */
    if (env->RegisterNatives(clazz, gMethods,
            sizeof(gMethods) / sizeof(gMethods[0])) != JNI_OK)
    {
        ALOGE("Failed registering methods for %s\n", kClassName);
        return -1;
    }

    /* fill out the rest of the ID cache */
    return 0;
}


