/***************************************************************************
 * Copyright                                                               *
 *                                                                         *
 *     ESCRYPT GmbH - Embedded Security       ESCRYPT Inc.                 *
 *     Zentrum fuer IT-Sicherheit             315 E Eisenhower Parkway     *
 *     Lise-Meitner-Allee 4                   Suite 214                    *
 *     44801 Bochum                           Ann Arbor, MI 48108          *
 *     Germany                                USA                          *
 *                                                                         *
 *     http://www.escrypt.com                                              *
 *     info"at"escrypt.com                                                 *
 *                                                                         *
 * All Rights reserved                                                     *
 ***************************************************************************/

/***************************************************************************/
/*!
   \file        jpake.h

   \brief       J-PAKE protocol implementation

   $Rev: 19842 $
 */
/***************************************************************************/

#ifndef _ESC_JPAKE_H_
#define _ESC_JPAKE_H_

/***************************************************************************
 * 1. INCLUDES                                                             *
 ***************************************************************************/

#include "cycurlib_config.h"
#include "ecc.h"
#include "sha_256.h"

#ifdef  __cplusplus
/* *INDENT-OFF* */
extern "C" {
 /* *INDENT-ON* */
#endif

/***************************************************************************
 * 2. DEFINES                                                              *
 ***************************************************************************/

#ifndef ID_LNG
/** Size of local ID in bytes */
#   define ID_LNG 4U
#endif

/* The following defines must not be changed! */

/* State values (exposed in the header for the tests) */

/** Initial state */
#define EscJPakeState_START             0x00U
/** Message for step 1 was generated */
#define EscJPakeState_STEP1_GENERATED   0x01U
/** Message received in step 1 was verified */
#define EscJPakeState_STEP1_VERIFIED    0x02U
/** Step 1 finished (message generated and message verified) */
#define EscJPakeState_STEP1_FINISHED    0x03U
/** Message for step 2 was generated */
#define EscJPakeState_STEP2_GENERATED   0x04U
/** Message received in step 2 was verified */
#define EscJPakeState_STEP2_VERIFIED    0x05U
/** Step 2 finished (message generated and message verified) */
#define EscJPakeState_STEP2_FINISHED    0x06U
/** Session key was computed */
#define EscJPakeState_KEY_COMPUTED      0x07U
/** Key confirmation M5 was computed */
#define EscJPakeState_M5_GENERATED      0x08U
/** Received key confirmation M5 was verified */
#define EscJPakeState_M5_VERIFIED       0x09U
/** Protocol is finished */
#define EscJPakeState_FINISHED          0x0AU

/* Error values */

/** No error occurred */
#define EscJPakeErr_NO_ERROR            0x00U
/** The zero-knowledge proof is invalid */
#define EscJPakeErr_INVALID_ZKP         0x01U
/** The key confirmation (M5 or M6) invalid */
#define EscJPakeErr_INVALID_CONFIRM     0x02U
/** A function was called in the wrong state */
#define EscJPakeErr_INVALID_STATE       0x03U
/** An EC point is infinity or not on the elliptic curve */
#define EscJPakeErr_INVALID_POINT       0x04U
/** An invalid random number was passed to a function (some random numbers may not be zero!) */
#define EscJPakeErr_INVALID_RANDOM      0x05U
/** An invalid parameter (e.g. a NULL pointer) was passed to a function */
#define EscJPakeErr_INVALID_PARAMETER   0x06U
/** The shared secret password may not be zero! **/
#define EscJPakeErr_INVALID_PASSWORD    0x07U
/** Internal hashing error */
#define EscJPakeErr_HASH_ERROR          0x08U


/** Field element length in bytes */
#define FE_LNG              ( ECC_KEY_BYTES )

/** Session key length in bytes. Equals one coordinate of the EC point. */
#define SESSION_KEY_LNG     ( FE_LNG )

/** Shared secret length in bytes. Equals one coordinate of the EC point. */
#define SHARED_SECRET_LNG   ( FE_LNG )

/**
 * Points have to be converted to UINT8 format for transmission.
 * each point contains 3 field elements
 */
#define POINT_LNG           ( FE_LNG * 3U )

/** Step 1 message length, format: id | X1 | V1 | r1 | X2 | V2 | r2 */
#define STEP1_MSG_LNG       ( ID_LNG + ( 2U * ( POINT_LNG + POINT_LNG + FE_LNG ) ) )

/** Step 2 message length, format: id | A | V | r */
#define STEP2_MSG_LNG       ( ID_LNG + POINT_LNG + POINT_LNG + FE_LNG )

/** Precomputed data length, format: X1 | V1 | r1 | X2 | V2 | r2 | x2 */
#define PRECOMP_LNG         ( ( 2U * ( POINT_LNG + POINT_LNG + FE_LNG ) ) + FE_LNG )

/***************************************************************************
 * 4. CONSTANTS                                                            *
 ***************************************************************************/

/***************************************************************************
 * 3. DECLARATIONS                                                         *
 ***************************************************************************/

/** Type for errors */
typedef UINT8 EscJPake_ErrorCode;

/** Type for internal states */
typedef UINT8 EscJPake_State;

/** Protocol context. */
typedef struct {
    EscJPake_State state;
    UINT8 localId[ID_LNG];
    EscEcc_PointT p_transmitted1;
    EscEcc_PointT p_transmitted2;
    EscEcc_PointT p_received1;
    EscEcc_PointT p_received2;
    EscEcc_PointT p_step2_received;
    EscEcc_FieldElementT fe_x2;
    EscEcc_FieldElementT fe_x2TimesS;
    UINT8 key[SESSION_KEY_LNG];
    UINT8 doubleHashedKey[EscSha256_DIGEST_LEN];
} EscJPake_ContextT;


/***************************************************************************
 * 5. FUNCTION PROTOTYPES                                                  *
 ***************************************************************************/

/**
 * Initialize the JPAKE context structure with the local ID.
 * The context MUST be initialized before it is passed to any other function!
 * 
 * \param[out]  context     Context structure to be initialized.
 * \param[in]   localId     Unique local ID of length ID_LNG.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode EscJPake_init(
    EscJPake_ContextT* context,
    const UINT8 localId[]);

/**
 * Precompute the data which is required for the first protocol message
 * to be sent. The precomputed data can be used in a function call to
 * EscJPake_usePrecStep1Mes().
 * 
 * \param[in]   randomX1    Random number of length FE_LNG.
 * \param[in]   randomV1    Random number of length FE_LNG.
 * \param[in]   randomX2    Random number of length FE_LNG. Must not be zero!
 * \param[in]   randomV2    Random number of length FE_LNG.
 * \param[in]   localId     Unique local ID of length ID_LNG.
 * \param[out]  precomputed Precomputed data set of length PRECOMP_LNG.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_precomputeData(
    const UINT8 randomX1[],
    const UINT8 randomV1[],
    const UINT8 randomX2[],
    const UINT8 randomV2[],
    const UINT8 localId[],
    UINT8 precomputed[]);

/**
 * Use precomputed data to generate the message for step 1 (i.e. message M1 or M2).
 * This function uses the data computed by EscJPake_precomputeData(). It is an
 * alternative to EscJPake_generateStep1Message() which computes the content of
 * the message on the fly.
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[in]       precomputed Data of length PRECOMP_LNG,
 *                              precomputed by EscJPake_precomputeData()
 * \param[out]      message     The message for step 1 of length STEP1_MSG_LNG.       
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_usePrecStep1Mes(
    EscJPake_ContextT *context,
    const UINT8 precomputed[],
    UINT8 message[]);

/**
 * Generate the message for step 1 (i.e. message M1 or M2).
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[in]       randomX1    Random number of length FE_LNG.
 * \param[in]       randomV1    Random number of length FE_LNG.
 * \param[in]       randomX2    Random number of length FE_LNG. Must not be zero!
 * \param[in]       randomV2    Random number of length FE_LNG.
 * \param[out]      message     The message for step 1 of length STEP1_MSG_LNG.       
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_generateStep1Message(
    EscJPake_ContextT* context,
    const UINT8 randomX1[],
    const UINT8 randomV1[],
    const UINT8 randomX2[],
    const UINT8 randomV2[],
    UINT8 message[]);

/**
 * Verify the content of a message received in step 1 (i.e. message M1 or M2).
 * The zero-knowledge proofs are verified and it is checked that the contained ID
 * is different from the local ID (to prevent reflection attacks). Also, the second
 * received point (X2 or X4, respectively) is tested to be unequal to infinity.
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[in]       message     The message received during step 1 of length STEP1_MSG_LNG.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_verifyStep1Message(
    EscJPake_ContextT* context,
    const UINT8 *message );

/**
 * Generate the message for step 2 (i.e. message M3 or M4).
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[in]       randomV     Random number of length FE_LNG. Used for the zero-knowledge proof.
 * \param[in]       password    The shared secret of the communication partners.
 *                              The length is SHARED_SECRET_LNG.
 * \param[out]      message     The message for step 2 of length STEP2_MSG_LNG.       
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_generateStep2Message(
    EscJPake_ContextT* context,
    const UINT8 randomV[],
    const UINT8 password[],
    UINT8 message[]);

/**
 * Verify the content of a message received in step 2 (i.e. message M3 or M4).
 * The zero-knowledge proof is verified and it is checked that the contained ID
 * is different from the local ID (to prevent reflection attacks).
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[in]       message     The message received during step 1 of length STEP2_MSG_LNG.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_verifyStep2Message(
    EscJPake_ContextT* context,
    const UINT8 message[]);

/**
 * Compute the session key. This function can be called after steps 1 and 2 are finished.
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[out]      key         The computed session key of length SESSION_KEY_LNG.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_computeKey(
    EscJPake_ContextT* context,
    UINT8 key[]);

/**
 * Generate the key confirmation message M5 for one of the communication partners.
 * Please note that the other communication partner must use EscJPake_generateConfirmationM6().
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[out]      message     The key confirmation message M5. The length is 32 bytes.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_generateConfirmationM5(
    EscJPake_ContextT* context,
    UINT8 message[]);

/**
 * Verify the received key confirmation message M5.
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[out]      message     The key confirmation message M5 to be verified. The length is 32 bytes.
 *
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_verifyConfirmationM5(
    EscJPake_ContextT* context,
    const UINT8 message[]);

/**
 * Generate the key confirmation message M6 for one of the communication partners.
 * Please note that the other communication partner must use EscJPake_generateConfirmationM5().
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[out]      message     The key confirmation message M6. The length is 32 bytes.
 * 
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_generateConfirmationM6(
    EscJPake_ContextT* context,
    UINT8 message[]);

/**
 * Verify the received key confirmation message M6.
 * 
 * \param[in,out]   context     Context structure which keeps track of the protocol status.
 * \param[out]      message     The key confirmation message M6 to be verified. The length is 32 bytes.
 *
 * \return EscJPakeErr_NO_ERROR on success or a corresponding error code otherwise.
 */
extern EscJPake_ErrorCode
EscJPake_verifyConfirmationM6(
    EscJPake_ContextT* context,
    const UINT8 message[]);

/***************************************************************************
 * 6. MACRO FUNCTIONS                                                      *
 ***************************************************************************/

#ifdef  __cplusplus
/* *INDENT-OFF* */
}
/* *INDENT-ON* */
#endif

#endif /* _ESC_JPAKE_H_ */

/***************************************************************************
 * 7. END                                                                  *
 ***************************************************************************/
