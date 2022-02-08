/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.cgmapp.application.safety.FixPointFloat
 * Brief: The class for convert the original number value to fix point value and
 * convert the fix point value to original value.
 *
 * Create Date: 5/14/2014
 * $Revision: 24176 $
 * $Author: StanleySu $
 * $Id: FixPointFloat.java 24176 2015-11-16 09:36:04Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;

import java.io.Serializable;
import java.math.BigDecimal;

public class FixPointFloat implements Serializable,
        ISafetyEquals<Integer, Integer>
{

    // The following two constants are for the operation of fix point float
    public static final int FIX_POINT_EXPONENT = 2;

    public static final BigDecimal FIX_POINT_FACTOR = BigDecimal.TEN
            .pow(FIX_POINT_EXPONENT);

    // serial Version UID
    // This ID is generaled by Android. DON't change to upper case
    private static final long serialVersionUID = 3972049161562409232L;

    // for storing original value
    private int mOriginal = 1;

    // for storing diverse value
    private int mDiverse = -1;

    /**
     * Construct a FixPointFloat with fix point value. It will call set function
     * to set the original value and diverse value.
     * 
     * @param nOriginal [in] the original fix point float value
     * @param nDiverse [in] the diverse fix point float value
     */
    public FixPointFloat(final int nOriginal, final int nDiverse)
    {
        set(nOriginal, nDiverse);
    }

    /**
     * Construct a FixPointFloat with original number value in SafetyFloat type.
     * It will call set function to convert the float value to fix point value.
     * 
     * @param value [in] the original number value in SafetyFloat type
     */
//    public FixPointFloat(SafetyFloat value)
//    {
//        set(value);
//    }
    
    public FixPointFloat(final float original, final String diverse)
    {
        set(original, diverse);
    }


    /**
     * Construct a FixPointFloat with original number value in SafetyNumber
     * type.
     * It will call set function to convert the integer value to fix point
     * value.
     * 
     * @param value [in] the original number value in SafetyNumber type
     */
    public FixPointFloat(final SafetyNumber<Integer> value)
    {
        set(value);
    }

    /**
     * Set the original fix point float and diverse fix point float and check
     * the integrity of original fix point float by safetyEquals() function.
     * Throw DataIntegrityException if check result of safetyEquals() is
     * SafetyBoolean.FALSE.
     * 
     * @param nOriginal [in] the original fix point float value
     * @param nDiverse [in] the diverse fix point float value
     * @return void [out]
     */
    public final void set(final int nOriginal, final int nDiverse)
    {
        mOriginal = nOriginal;
        mDiverse = nDiverse;

        // compare the result
        final SafetyBoolean isEquals = safetyEquals(mOriginal, mDiverse);
        
        if (isEquals.getByte() != SafetyBoolean.TRUE.getByte())
        {
            throw new DataIntegrityException("original value ["
                    + mOriginal + "] is different from the diverse value ["
                    + mDiverse + " ]");
        }
    }

    /**
     * Set the fix point float by passed in SafetyFloat value.<br />
     * 
     * It will convert the float value to fix point value and check the
     * integrity of convert result by safetyEquals() function. Throw
     * DataIntegrityException if check result of safetyEquals() is
     * SafetyBoolean.FALSE.
     * 
     * @param value [in] the original number value in SafetyFloat type
     * @return void [out]
     */
//    public void set(SafetyFloat value)
//    {
//        float fValue = value.getOriginal();
//        String stringValue = value.getDiverse();
//
//        // round prevent imprecise float
//        int nOriginal = new BigDecimal(fValue).multiply(FIX_POINT_FACTOR)
//                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();
//        int nDiverse = new BigDecimal(stringValue).negate()
//                .multiply(FIX_POINT_FACTOR)
//                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();
//
//        set(nOriginal, nDiverse);
//    }
    
    public final void set(final float original, final String diverse)
    {
        // round prevent imprecise float
        final int nOriginal = new BigDecimal(original).multiply(FIX_POINT_FACTOR)
                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();
        final int nDiverse = new BigDecimal(diverse).negate()
                .multiply(FIX_POINT_FACTOR)
                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();

        set(nOriginal, nDiverse);
    }


    /**
     * Set the fix point float by passed in SafetyNumber value.<br />
     * 
     * It will convert the integer value to fix point value and check the
     * integrity of convert result by safetyEquals() function. Throw
     * DataIntegrityException if check result of safetyEquals() is
     * SafetyBoolean.FALSE.
     * 
     * @param value [in] the original number value in SafetyNumber type
     * @return void [out]
     */
    public final void set(final SafetyNumber<Integer> value)
    {
        final int nOriginalValue = value.getOriginal();
        final int nDiverseValue = value.getDiverse();

        final int nOriginal = nOriginalValue * FIX_POINT_FACTOR.intValue();
        final int nDiverse = nDiverseValue * FIX_POINT_FACTOR.intValue();

        set(nOriginal, nDiverse);
    }

    /**
     * Check the data integrity of original fix point float by safetyEquals()
     * function and return the original data if the check result is
     * SafetyBoolean.TRUE, otherwise throw DataIntegrityException.
     * 
     * @return int [out] the original fix point float value
     */
    public final int get()
    {
        final int nResult = mOriginal;

        final SafetyBoolean isEquals = safetyEquals(nResult, mDiverse);

        if (isEquals.getByte() != SafetyBoolean.TRUE.getByte())
        {
            throw new DataIntegrityException();
        }

        return nResult;
    }

    /**
     * Return the original fix point float value without the safety comparison.
     * 
     * @return int [out] the original fix point float value
     */
    public final int getOriginal()
    {
        return mOriginal;
    }

    /**
     * Return the diverse fix point float value without the safety comparison.
     * 
     * @return int [out] the diverse fix point float value
     */
    public final int getDiverse()
    {
        return mDiverse;
    }

    /**
     * Convert the fix point value to original number value in SafetyNumber
     * type.<br />
     * 
     * It will throw DataIntegrityException if check the integrity of original
     * fix point float value failed or check the integrity of convert result
     * failed.
     * 
     * @return SafetyNumber<Integer> [out] the SafetyNumber value that convert
     *         from fix point float value
     */
    public final SafetyNumber<Integer> getSafetyNumber()
    {
        final int nOriginalValue = new BigDecimal(mOriginal).divide(FIX_POINT_FACTOR)
                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();

        final int nDiverseValue = new BigDecimal(-mDiverse).divide(FIX_POINT_FACTOR)
                .setScale(0, BigDecimal.ROUND_HALF_UP).intValue();

        final SafetyNumber<Integer> result = new SafetyNumber<Integer>(nOriginalValue,
                -nDiverseValue);

        final SafetyBoolean isEquals = safetyEquals(mOriginal, mDiverse);

        if (isEquals.getByte() == SafetyBoolean.FALSE.getByte())
        {
            throw new DataIntegrityException("the original value [" + mOriginal
                    + "] is different from diverse value [" + mDiverse + "]");
        }

        return result;
    }

    /**
     * Convert the fix point value to original number value in SafetyFloat type. <br />
     * 
     * It will throw DataIntegrityException if check the integrity of original
     * fix point float value failed or check the integrity of convert result
     * failed.
     * 
     * @return SafetyFloat [out] the SafetyFloat value that convert from fix
     *         point float value
     */
//    public SafetyFloat getSafetyFloat()
//    {
//        float fOriginalValue = new BigDecimal(mOriginal)
//                .divide(FIX_POINT_FACTOR).setScale(1, BigDecimal.ROUND_HALF_UP)
//                .floatValue();
//
//        float fDiverseValue = new BigDecimal(-mDiverse).divide(FIX_POINT_FACTOR)
//                .setScale(1, BigDecimal.ROUND_HALF_UP).floatValue();
//
//        SafetyFloat result = new SafetyFloat(fOriginalValue, String.format(
//                Locale.US, "%.1f", fDiverseValue));
//
//        SafetyBoolean isEquals = safetyEquals(mOriginal, mDiverse);
//
//        if (isEquals.getByte() == SafetyBoolean.FALSE.getByte())
//        {
//            throw new DataIntegrityException("the original value [" + mOriginal
//                    + "] is different from diverse value [" + mDiverse + "]");
//        }
//
//        return result;
//    }
    public final float getOriginFloat()
    {
        final float fOriginalValue = new BigDecimal(mOriginal)
                .divide(FIX_POINT_FACTOR).setScale(1, BigDecimal.ROUND_HALF_UP)
                .floatValue();

        final SafetyBoolean isEquals = safetyEquals(mOriginal, mDiverse);

        if (isEquals.getByte() == SafetyBoolean.FALSE.getByte())
        {
            throw new DataIntegrityException("the original value [" + mOriginal
                    + "] is different from diverse value [" + mDiverse + "]");
        }

        return fOriginalValue;
    }


    /**
     * Return SafetyBoolean.TRUE if the original fix point float value is equals
     * to the negative value of diverse fix point float. Otherwise return
     * SafetyBoolean.FALSE.
     * 
     * @param original [in] The original fix point float data.
     * @param diverse [in] The diverse fix point float data.
     * @return
     */
    @Override
    public final SafetyBoolean safetyEquals(final Integer original, final Integer diverse)
    {
        SafetyBoolean isResultOK = SafetyBoolean.FALSE;

        if (original.equals(-diverse))
        {
            isResultOK = SafetyBoolean.TRUE;
        }

        return isResultOK;
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-555
// [Comment]: Implements the Fix Point method to replace the floating point
// calculation and Safety for the bG, Insulin, Carbs data processing.
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix "CMP.OBJ" issues.
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]:ATOS-267
// [Comment]:coding guideline
// (10801 2014-06-09 09:54:09Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Preform KEYWORD function of SVN for each source files. No Source
// code content will be changed
// (21021 2014-10-03 02:34:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (21467 2014-10-07 14:22:36Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Update for comply the coding guideline
// (21485 2014-10-08 05:38:44Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline - Fix rule R17, R88, R100, R102, etc.
