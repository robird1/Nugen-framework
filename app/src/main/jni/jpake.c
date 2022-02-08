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
   \file        jpake.c

   \brief       J-PAKE protocol implementation

   $Rev: 19842 $
 */
/***************************************************************************/
/***************************************************************************
* 1. INCLUDES                                                              *
****************************************************************************/

#include "jpake.h"


/***************************************************************************
 * 2. DEFINES                                                              *
 ***************************************************************************/

/** Length of zero-knowledge proof without ID */
#define ZKP_LNG ( POINT_LNG + FE_LNG )

/** Maximum of hash length and field element length */
#define MAX_HASH_FE_LENGTH  ( ( EscSha256_DIGEST_LEN > FE_LNG ) ? EscSha256_DIGEST_LEN : FE_LNG )

/***************************************************************************
 * 3. DECLARATIONS                                                         *
 ***************************************************************************/

/***************************************************************************
 * 4. CONSTANTS                                                            *
 ***************************************************************************/

/***************************************************************************
 * 5. FUNCTION PROTOTYPES                                                  *
 ***************************************************************************/

static EscJPake_ErrorCode
EscJPake_computeZKP(
    const UINT8 localId[],
    const EscEcc_FieldElementT* x,
    const UINT8 randomV[],
    const EscEcc_PointT* G,
    const EscEcc_PointT* X,
    UINT8 zkp[]);

static EscJPake_ErrorCode
EscJPake_verifyZKP(
    const UINT8 id[],
    const EscEcc_PointT* G,
    const EscEcc_PointT* X,
    const UINT8 zkp[]);

static EscJPake_ErrorCode
EscJPake_tripleHash(
        EscJPake_ContextT* context,
        UINT8 message[]);

/***************************************************************************
 * 6. IMPLEMENTATION OF FUNCTIONS                                          *
 ***************************************************************************/

/*
 * computes ZKP of randomX
 * attaches V and r to message at pointer idx (idx = START of X)
 */
static EscJPake_ErrorCode
EscJPake_computeZKP(
    const UINT8 localId[],
    const EscEcc_FieldElementT* x,
    const UINT8 randomV[],
    const EscEcc_PointT* G,
    const EscEcc_PointT* X,
    UINT8 zkp[])
{
    EscJPake_ErrorCode status = EscJPakeErr_NO_ERROR;

    EscSha256_ContextT hash_ctx;
    EscEcc_FieldElementT v;
    EscEcc_FieldElementT fe_h;
    EscEcc_FieldElementT tmp1, tmp2;
    EscEcc_FieldElementLongT v_long;
    EscEcc_PointT V;
    UINT8 hashValue[MAX_HASH_FE_LENGTH];
    UINT8 uint8_tmp[POINT_LNG];
    BOOL hashFailed;

    /* get field element from random byte array */
    EscEccFe_FromUint8(randomV, &v);

    /* verify that randomV is in [0..n] */
    if ( (EscEccFe_AbsoluteCompare( &v, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
        EscEccFe_ToLongElement(&v_long, &v);
        EscEccFe_Reduce(&v, &v_long, &EscEcc_curve32.base_point_order_n);
    }

    /* perform hashing: h = H( G || V || X || ID ) */

    /* init hashing */
    hashFailed = EscSha256_Init(&hash_ctx);

    if (hashFailed == FALSE) {
        /* hash base point G */
        EscEccPt_ToUint8(G, uint8_tmp);
        hashFailed = EscSha256_Update(&hash_ctx, uint8_tmp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* compute V */
        EscEccPt_JacobianMultiplyBinary(&V, &v, G);
        /* hash V */
        EscEccPt_ToUint8(&V, uint8_tmp);
        hashFailed = EscSha256_Update(&hash_ctx, uint8_tmp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* hash X */
        EscEccPt_ToUint8(X, uint8_tmp);
        hashFailed = EscSha256_Update(&hash_ctx, uint8_tmp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* hash Id */
        hashFailed = EscSha256_Update(&hash_ctx, localId, ID_LNG);
    }

    if (hashFailed == FALSE) {
        /* finalize hashing  */
        hashFailed = EscSha256_Finish(&hash_ctx, hashValue);
    }

    if (hashFailed) {
        status = EscJPakeErr_HASH_ERROR;
    }
    else {
        /*** compute r = v - xh ***/

        /* get field element from hash value */
#if (EscSha256_DIGEST_LEN < FE_LNG)
        UINT32 i;
        for (i=EscSha256_DIGEST_LEN; i<FE_LNG; i++) {
            hashValue[i] = 0U;
        }
#endif
        /*lint -save -esym(645,hashValue) hashValue is initialized */
        EscEccFe_FromUint8(hashValue, &fe_h);
        /*lint -restore */

        /* compute x*h */
        EscEccFe_ModularMultiply( &tmp2, x, &fe_h, &EscEcc_curve32.base_point_order_n );
        /* compute  v - xh */
        EscEccFe_ModularSub( &tmp1, &v, &tmp2, &EscEcc_curve32.base_point_order_n );

        /* update ZKP */
        /*lint -save -esym(645,V) V is initialized */
        EscEccPt_ToUint8(&V, zkp);
        /*lint -restore */
        EscEccFe_ToUint8(&tmp1, &zkp[POINT_LNG]);
    }

    return status;
}

/*
 * reads V and r from zkp
 * computes h' and V'
 * verifies that V = V'
  */
static EscJPake_ErrorCode
EscJPake_verifyZKP(
    const UINT8 id[],
    const EscEcc_PointT* G,
    const EscEcc_PointT* X,
    const UINT8 zkp[])
{
    EscJPake_ErrorCode status = EscJPakeErr_NO_ERROR;
    EscSha256_ContextT hash_ctx;
    EscEcc_FieldElementT fe_r;
    EscEcc_FieldElementT fe_h;
    EscEcc_PointT receivedV;
    EscEcc_PointT computedV;
    UINT8 hashValue[MAX_HASH_FE_LENGTH];
    UINT8 uint8_tmp[POINT_LNG];

    BOOL hashFailed;

    /* perform hashing: h = H( G || V || X || ID ) */

    /* init hashing */
    hashFailed = EscSha256_Init(&hash_ctx);

    if (hashFailed == FALSE) {
        /* hash base point G */
        EscEccPt_ToUint8(G, uint8_tmp);
        hashFailed = EscSha256_Update(&hash_ctx, uint8_tmp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* hash V */
        hashFailed = EscSha256_Update(&hash_ctx, zkp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* hash X */
        EscEccPt_ToUint8(X, uint8_tmp);
        hashFailed = EscSha256_Update(&hash_ctx, uint8_tmp, POINT_LNG);
    }

    if (hashFailed == FALSE) {
        /* hash Id */
        hashFailed = EscSha256_Update(&hash_ctx, id, ID_LNG);
    }

    if (hashFailed == FALSE) {
        /* finalize hashing  */
        hashFailed = EscSha256_Finish(&hash_ctx, hashValue);
    }

    if (hashFailed == FALSE) {
        /* extract V and r from message */
        EscEccPt_FromUint8(zkp, &receivedV);
        /* verify received point  */
        if ( EscEccPt_Check(&receivedV) ) {
            status = EscJPakeErr_INVALID_ZKP;
        }
        else {
            EscEccFe_FromUint8(&zkp[POINT_LNG], &fe_r);
            /* verify that r < n */
            if ( (EscEccFe_AbsoluteCompare( &fe_r, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
                status = EscJPakeErr_INVALID_ZKP;
            }
            else {
                /* get field element from hash value */
#if (EscSha256_DIGEST_LEN < FE_LNG)
                UINT32 i;
                for (i=EscSha256_DIGEST_LEN; i<FE_LNG; i++) {
                    hashValue[i] = 0U;
                }
#endif
                /*lint -save -esym(645,hashValue) hashValue is initialized */
                EscEccFe_FromUint8(hashValue, &fe_h);
                /*lint -restore */

                /* compute V' = G*r + X*h */
                EscEccPt_JacDualMulAddBin(&computedV, &fe_r, G, &fe_h, X);

                /* compare receivedV and computedV */
                if (EscEccPt_IsEqual(&receivedV, &computedV) == FALSE) {
                    status = EscJPakeErr_INVALID_ZKP;
                }
            }
        }
    }
    else {
        status = EscJPakeErr_HASH_ERROR;
    }

    return status;
}


static EscJPake_ErrorCode
EscJPake_tripleHash(
        EscJPake_ContextT* context,
        UINT8 message[])
{
    static const UINT8 confirm[] = "ok";

    EscJPake_ErrorCode status = EscJPakeErr_NO_ERROR;
    EscSha256_ContextT hash_ctx;
    UINT8 hashValue[EscSha256_DIGEST_LEN];
    BOOL hasFailed;

    /******** compute H(H(H( K | confirm ))) ********/

    /* initialize hashing round 1 */
    hasFailed = EscSha256_Init(&hash_ctx);
    if ( hasFailed == FALSE ) {
        /* hash key */
        hasFailed = EscSha256_Update(&hash_ctx, context->key, SESSION_KEY_LNG);
    }
    if ( hasFailed == FALSE ) {
        /* hash confirm message */
        hasFailed = EscSha256_Update(&hash_ctx, confirm, sizeof(confirm));
    }
    if ( hasFailed == FALSE ) {
        /* finalize hashing round 1 */
        hasFailed = EscSha256_Finish(&hash_ctx, hashValue);
    }
    if ( hasFailed == FALSE ) {
        /* hashing round 2 -> store hash value in context for verification of M6 */
        hasFailed = EscSha256_Calc(hashValue, EscSha256_DIGEST_LEN, context->doubleHashedKey);
    }
    if ( hasFailed == FALSE ) {
        /* hashing round 3 */
        hasFailed = EscSha256_Calc(context->doubleHashedKey, EscSha256_DIGEST_LEN, message);
    }

    if ( hasFailed == TRUE ) {
        status = EscJPakeErr_HASH_ERROR;
    }

    return status;
}

EscJPake_ErrorCode EscJPake_init(
    EscJPake_ContextT* context,
    const UINT8 localId[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) && (localId != 0) ) {
        UINT32 i;

        EscEccPt_SetInfinity(&context->p_transmitted1);
        EscEccPt_SetInfinity(&context->p_transmitted2);
        EscEccPt_SetInfinity(&context->p_received1);
        EscEccPt_SetInfinity(&context->p_received2);
        EscEccFe_SetZero(&context->fe_x2);
        EscEccFe_SetZero(&context->fe_x2TimesS);

        for (i=0U; i<ID_LNG; i++) {
            context->localId[i] = localId[i];
        }

        for (i=0U; i<SESSION_KEY_LNG; i++) {
            context->key[i] = 0U;
        }

        for (i=0U; i<EscSha256_DIGEST_LEN; i++) {
            context->doubleHashedKey[i] = 0U;
        }

        status = EscJPakeErr_NO_ERROR;
        context->state = EscJPakeState_START;
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_precomputeData(
    const UINT8 randomX1[],
    const UINT8 randomV1[],
    const UINT8 randomX2[],
    const UINT8 randomV2[],
    const UINT8 localId[],
    UINT8 precomputed[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (randomX1 != 0) && (randomX2 != 0) && (randomV1 != 0) && (randomV2 != 0) &&  (localId != 0) && (precomputed != 0) ) {
        EscEcc_FieldElementT x1;
        EscEcc_FieldElementT x2;
        EscEcc_FieldElementLongT x_long;

        /* get field elements x1 and x2 from random bytes */
        EscEccFe_FromUint8(randomX1, &x1);
        EscEccFe_FromUint8(randomX2, &x2);
        
        /* check if x1 < n -> otherwise compute x1 mod n */
        if ( (EscEccFe_AbsoluteCompare( &x1, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
            EscEccFe_ToLongElement(&x_long, &x1);
            EscEccFe_Reduce(&x1, &x_long, &EscEcc_curve32.base_point_order_n);
        }

        /* check if x2 < n -> otherwise compute x2 mod n */
        if ( (EscEccFe_AbsoluteCompare( &x2, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
            EscEccFe_ToLongElement(&x_long, &x2);
            EscEccFe_Reduce(&x2, &x_long, &EscEcc_curve32.base_point_order_n);
        }

        /* verify that x2 != 0 */
        if ( EscEccFe_IsZero(&x2) == FALSE )  {  /* x2 < n */
            EscEcc_PointT X1;
            EscEcc_PointT X2;
            UINT32 idx = 0U;

            /* compute X1 and X2 */
            EscEccPt_JacobianMultiplyBinary(&X1, &x1, &EscEcc_curve32.base_point_G);
            EscEccPt_JacobianMultiplyBinary(&X2, &x2, &EscEcc_curve32.base_point_G);

            /* store X1 in message */
            EscEccPt_ToUint8(&X1, &precomputed[idx]);
            idx += POINT_LNG;

            /* compute ZKP(x1) and store in message */
            status = EscJPake_computeZKP(localId, &x1, randomV1, &EscEcc_curve32.base_point_G, &X1, &precomputed[idx]);
            idx += ZKP_LNG;

            if (status == EscJPakeErr_NO_ERROR) {
                /* store X2 in message */
                EscEccPt_ToUint8(&X2, &precomputed[idx]);
                idx += POINT_LNG;

                /* compute ZKP(x2) and store in message*/
                status = EscJPake_computeZKP(localId, &x2, randomV2, &EscEcc_curve32.base_point_G, &X2, &precomputed[idx]);
                idx += ZKP_LNG;

                if (status == EscJPakeErr_NO_ERROR) {
                    /* append x2 */
                    EscEccFe_ToUint8(&x2, &precomputed[idx]);
                }
            }
        }
        else {
            status = EscJPakeErr_INVALID_RANDOM;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_usePrecStep1Mes(
    EscJPake_ContextT *context,
    const UINT8 precomputed[],
    UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (message != 0) && (precomputed != 0) ) {
        if ( (context->state == EscJPakeState_START) || (context->state == EscJPakeState_STEP1_VERIFIED) ) {
            UINT32 i;

            status = EscJPakeErr_NO_ERROR;

            /* update context */
            /* X1 */
            EscEccPt_FromUint8(&precomputed[0], &context->p_transmitted1);
            /* X2 */
            EscEccPt_FromUint8(&precomputed[POINT_LNG + ZKP_LNG], &context->p_transmitted2);
            /* x2 */
            EscEccFe_FromUint8(&precomputed[POINT_LNG + ZKP_LNG + POINT_LNG + ZKP_LNG], &context->fe_x2);

            /* set ID in message */
            for (i=0U; i<ID_LNG; i++) {
                message[i] = context->localId[i];
            }
            
            /* set precomputed part in message */
            for (i = 0U; i < (PRECOMP_LNG - FE_LNG); i++) {
                message[ID_LNG + i] = precomputed[i];
            }

            /* transition to next state */
            if (context->state == EscJPakeState_START) {
                /* start -> step 1 generated */
                context->state = EscJPakeState_STEP1_GENERATED;
            }
            else {
                /* step 1 verified -> step 1 finished */
                Esc_ASSERT(context->state == EscJPakeState_STEP1_VERIFIED);
                context->state = EscJPakeState_STEP1_FINISHED;
            }
        }
        else  {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;    
}

EscJPake_ErrorCode
EscJPake_generateStep1Message(
    EscJPake_ContextT* context,
    const UINT8 randomX1[],
    const UINT8 randomV1[],
    const UINT8 randomX2[],
    const UINT8 randomV2[],
    UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&(randomX1 != 0) && (randomX2 != 0) && (randomV1 != 0) && (randomV2 != 0) &&  (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if ( (context->state == EscJPakeState_START) || (context->state == EscJPakeState_STEP1_VERIFIED) ) {
            EscEcc_FieldElementT x1;
            EscEcc_FieldElementLongT x_long;
            UINT32 idx;

            /* get field elements x1 and x2 from random bytes */
            EscEccFe_FromUint8(randomX1, &x1);
            EscEccFe_FromUint8(randomX2, &context->fe_x2);

            /* check if x1 < n -> otherwise compute x1 mod n */
            if ( (EscEccFe_AbsoluteCompare( &x1, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
                EscEccFe_ToLongElement(&x_long, &x1);
                EscEccFe_Reduce(&x1, &x_long, &EscEcc_curve32.base_point_order_n);
            }

            /* check if x2 < n -> otherwise compute x2 mod n */
            if ( (EscEccFe_AbsoluteCompare( &context->fe_x2, &EscEcc_curve32.base_point_order_n.prime_p ) == 1) ) {
                EscEccFe_ToLongElement(&x_long, &context->fe_x2);
                EscEccFe_Reduce(&context->fe_x2, &x_long, &EscEcc_curve32.base_point_order_n);
            }

            /* verify that x2 != 0 */
            if ( EscEccFe_IsZero(&context->fe_x2) == FALSE )  {

                /* store id in message */
                for (idx=0U; idx<ID_LNG; idx++)  {
                    message[idx] = context->localId[idx];
                }

                /* compute X1 and X2 and store in context */
                EscEccPt_JacobianMultiplyBinary(&context->p_transmitted1, &x1, &EscEcc_curve32.base_point_G);
                EscEccPt_JacobianMultiplyBinary(&context->p_transmitted2, &context->fe_x2, &EscEcc_curve32.base_point_G);

                /* store X1 in message */
                EscEccPt_ToUint8(&context->p_transmitted1, &message[idx]);
                idx += POINT_LNG;

                /* compute ZKP(x1) and store in message */
                status = EscJPake_computeZKP(context->localId, &x1, randomV1, &EscEcc_curve32.base_point_G, &context->p_transmitted1, &message[idx]);
                idx += POINT_LNG + FE_LNG;

                if (status == EscJPakeErr_NO_ERROR) {
                    /* store X2 in message */
                    EscEccPt_ToUint8(&context->p_transmitted2, &message[idx]);
                    idx += POINT_LNG;

                    /* compute ZKP(x2) and store in message*/
                    status = EscJPake_computeZKP(context->localId, &context->fe_x2, randomV2, &EscEcc_curve32.base_point_G, &context->p_transmitted2, &message[idx]);
                }

                /* transition to next state depending on current state */
                if (status == EscJPakeErr_NO_ERROR) {
                    if (context->state == EscJPakeState_START) {
                        /* start -> step 1 generated */
                        context->state = EscJPakeState_STEP1_GENERATED;
                    }
                    else {
                        /* step 1 verified -> step 1 finished */
                        Esc_ASSERT(context->state == EscJPakeState_STEP1_VERIFIED);
                        context->state = EscJPakeState_STEP1_FINISHED;
                    }
                }
            }
            else {
                status = EscJPakeErr_INVALID_RANDOM;
            }
        }
        else  {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_verifyStep1Message(
    EscJPake_ContextT* context,
    const UINT8 message[] )
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if ( (context->state == EscJPakeState_START) || (context->state == EscJPakeState_STEP1_GENERATED) ) {
            UINT8 id[ID_LNG];
            UINT32 i;

            /* extract values from message */
            /* STEP1 MESSAGE FORMAT: id | X3 | V3 | r3 | X4 | V4 | r4 */
            /* id */
            for (i=0U; i<ID_LNG; i++) {
                id[i] = message[i];
            }

            /* X3 */
            EscEccPt_FromUint8(&message[ID_LNG], &context->p_received1);
            /* X4 */
            EscEccPt_FromUint8(&message[ID_LNG + POINT_LNG + ZKP_LNG], &context->p_received2);

            /* verify received points */
            /*lint -save -esym(960, 12.4) EscEccPt_Check() has no side effects */
            if ( (EscEccPt_Check(&context->p_received1) == TRUE) || (EscEccPt_Check(&context->p_received2) == TRUE) ) {
                status = EscJPakeErr_INVALID_POINT;
            }
            /*lint -restore EscEccPt_Check() has no side effects */

            if (status == EscJPakeErr_NO_ERROR) {
                /* verify ZKP(x3) */
                status = EscJPake_verifyZKP(id, &EscEcc_curve32.base_point_G, &context->p_received1, &message[ID_LNG + POINT_LNG]);
                if ( status == EscJPakeErr_NO_ERROR ) {
                    /* verify ZKP(x4) */
                    status = EscJPake_verifyZKP(id, &EscEcc_curve32.base_point_G, &context->p_received2, &message[STEP1_MSG_LNG - ZKP_LNG]);
                }
            }

            /* transition to next state depending on current state */
            if (status == EscJPakeErr_NO_ERROR) {
                if (context->state == EscJPakeState_START) {
                    /* start -> step 1 generated */
                    context->state = EscJPakeState_STEP1_VERIFIED;
                }
                else {
                    /* step 1 generated -> step 1 verified */
                    Esc_ASSERT(context->state == EscJPakeState_STEP1_GENERATED);
                    context->state = EscJPakeState_STEP1_FINISHED;
                }
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_generateStep2Message(
    EscJPake_ContextT* context,
    const UINT8 randomV[],
    const UINT8 password[],
    UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) && (message != 0) && (randomV != 0) && (password != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if ( (context->state == EscJPakeState_STEP1_FINISHED) || (context->state == EscJPakeState_STEP2_VERIFIED) ) {
            EscEcc_PointT A;
            EscEcc_PointT T;
            EscEcc_FieldElementT s;
            UINT32 idx;

            /* store id in message */
            for (idx=0U; idx<ID_LNG; idx++) {
                message[idx] = context->localId[idx];
            }

            /* check password != 0 */
            EscEccFe_FromUint8(password, &s);
            if ( EscEccFe_IsZero(&s) ) {
                status = EscJPakeErr_INVALID_PASSWORD;
            }
            else {
                /* calculate x2 * s (s = password) and store in context */
                EscEccFe_FromUint8(password, &s);
                EscEccFe_ModularMultiply( &context->fe_x2TimesS, &context->fe_x2, &s, &EscEcc_curve32.base_point_order_n );

                /* calculate A = (X1 + X3 + X4) * x2*s */
                EscEccPt_JacobianAdd(&T, &context->p_received1, &context->p_received2);
                EscEccPt_JacobianAdd(&T, &T, &context->p_transmitted1); /* T = (X1 + X3 + X4) */
                EscEccPt_JacobianMultiplyBinary(&A, &context->fe_x2TimesS, &T);

                /* store A in message */
                EscEccPt_ToUint8(&A, &message[idx]);
                idx += POINT_LNG;

                /* get ZKP(x2 * s) and store in message */
                status = EscJPake_computeZKP(context->localId, &context->fe_x2TimesS, randomV, &T, &A, &message[idx]);

                if (status == EscJPakeErr_NO_ERROR) {
                    /* transition to next state depending on current state */
                    if (context->state == EscJPakeState_STEP1_FINISHED) {
                        /* step 1 finished -> step 2 generated */
                        context->state = EscJPakeState_STEP2_GENERATED;
                    }
                    else {
                        /* step 2 verified -> step 2 finished */
                        Esc_ASSERT(context->state == EscJPakeState_STEP2_VERIFIED);
                        context->state = EscJPakeState_STEP2_FINISHED;
                    }
                }
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_verifyStep2Message(
    EscJPake_ContextT* context,
    const UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) && (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if( (context->state == EscJPakeState_STEP1_FINISHED) || (context->state == EscJPakeState_STEP2_GENERATED) ) {
            EscEcc_PointT T;
            UINT8 id[ID_LNG];
            UINT32 idx;

            /******** STEP2 MESSAGE FORMAT: id | A | V | r ********/

            /* get id */
            for (idx=0U; idx<ID_LNG; idx++) {
                id[idx] = message[idx];
            }
            
            /* store A in context */
            EscEccPt_FromUint8(&message[ID_LNG], &context->p_step2_received);

            /* verify received point */
            if ( EscEccPt_Check(&context->p_step2_received) ) {
                status = EscJPakeErr_INVALID_POINT;
            }
            else {
                /* calculate T = (X1 + X2 + X3) */
                EscEccPt_JacobianAdd(&T, &context->p_transmitted1, &context->p_transmitted2);
                EscEccPt_JacobianAdd(&T, &T, &context->p_received1);

                /* verify ZKP(x2*s) */
                status = EscJPake_verifyZKP(id, &T, &context->p_step2_received, &message[ID_LNG + POINT_LNG]);

                if (status == EscJPakeErr_NO_ERROR) {
                    /* transition to next state depending on current state */
                    if (context->state == EscJPakeState_STEP1_FINISHED) {
                        /* step 1 finished -> step 2 verified */
                        context->state = EscJPakeState_STEP2_VERIFIED;
                    }
                    else {
                        /* step 2 generated -> step 2 finished */
                        Esc_ASSERT(context->state == EscJPakeState_STEP2_GENERATED);
                        context->state = EscJPakeState_STEP2_FINISHED;
                    }
                }
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_computeKey(
    EscJPake_ContextT* context,
    UINT8 key[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (key != 0) ) {
        if ( context->state == EscJPakeState_STEP2_FINISHED ) {
            EscEcc_PointT K;

            /* K = X4 * x2 * s */
            EscEccPt_JacobianMultiplyBinary(&K, &context->fe_x2TimesS, &context->p_received2);
            /* K = B - (X4 * x2 * s) */
            EscEccPt_JacobianSub(&K, &context->p_step2_received, &K);
            /* K = (B - X4 * x2 * s) * x2 */
            EscEccPt_JacobianMultiplyBinary(&K, &context->fe_x2, &K);

            EscEccPt_ToAffine(&K, &K);

            EscEccFe_ToUint8(&K.x, key);
            EscEccFe_ToUint8(&K.x, context->key);

            status = EscJPakeErr_NO_ERROR;
            context->state = EscJPakeState_KEY_COMPUTED;
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_generateConfirmationM5(
    EscJPake_ContextT* context,
    UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    /* check pointer */
    if ( (context != 0) &&  (message != 0) ) {
        /* check required state */
        if ( context->state == EscJPakeState_KEY_COMPUTED ) {
            /* generate M5 */
            status = EscJPake_tripleHash(context, message);
            if (status == EscJPakeErr_NO_ERROR) {
                context->state = EscJPakeState_M5_GENERATED;
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_verifyConfirmationM5(
    EscJPake_ContextT* context,
    const UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if (context->state == EscJPakeState_KEY_COMPUTED) {
            UINT8 referenceMessage[EscSha256_DIGEST_LEN];

            /* compute message M5 -> doubleHashedKey of context is updated within */
            status = EscJPake_tripleHash(context, referenceMessage);

            if (status == EscJPakeErr_NO_ERROR) {
                /* compare referenceMessage to received message */
                UINT32 i;
                for (i=0U; i<EscSha256_DIGEST_LEN; i++) {
                    if (referenceMessage[i] != message[i]) {
                        status = EscJPakeErr_INVALID_CONFIRM;
                        break;
                    }
                }
            }

            if (status == EscJPakeErr_NO_ERROR) {
                /* set state */
                context->state = EscJPakeState_M5_VERIFIED;
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
            context->state = EscJPakeState_KEY_COMPUTED;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_generateConfirmationM6(
    EscJPake_ContextT* context,
    UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if ( context->state == EscJPakeState_M5_VERIFIED ) {
            UINT32 i;

            /******** STEP3 MESSAGE M6: H(H(K)) ********/

            /* H(H(K)) has already been computed in M5 generation or verification */
            for (i=0U; i<EscSha256_DIGEST_LEN; i++) {
                message[i] = context->doubleHashedKey[i];
            }

            /* set state */
            context->state = EscJPakeState_FINISHED;
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}

EscJPake_ErrorCode
EscJPake_verifyConfirmationM6(
    EscJPake_ContextT* context,
    const UINT8 message[])
{
    EscJPake_ErrorCode status = EscJPakeErr_INVALID_PARAMETER;

    if ( (context != 0) &&  (message != 0) ) {
        status = EscJPakeErr_NO_ERROR;

        if ( context->state == EscJPakeState_M5_GENERATED ) {
            /* compare stored doubleHashedKey in context to received message */
            UINT32 i;
            for (i=0U; i<EscSha256_DIGEST_LEN; i++) {
                if (context->doubleHashedKey[i] != message[i]) {
                    status = EscJPakeErr_INVALID_CONFIRM;
                    break;
                }
            }

            if (status == EscJPakeErr_NO_ERROR) {
                /* set state */
                context->state = EscJPakeState_FINISHED;
            }
        }
        else {
            status = EscJPakeErr_INVALID_STATE;
        }
    }

    return status;
}


/***************************************************************************
 * 6. END                                                                  *
 ***************************************************************************/
