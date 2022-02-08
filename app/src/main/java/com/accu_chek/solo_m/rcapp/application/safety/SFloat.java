/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.cgmapp.communication.profile.data.SFloat
 * Brief: The SFloat is cG and trend format which is according to the IEEE-11073
 * 16-bit SFLOAT.
 *
 * Create Date: 12/25/2013
 * $Revision: 24205 $
 * $Author: StanleySu $
 * $Id: SFloat.java 24205 2015-11-16 10:29:28Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;

/**
 * IEEE-11073 16-bit SFLOAT
 * */
public class SFloat
{
    
    private static final int MASK_VALUE_000F = 0x000F;
    
    private static final int MASK_VALUE_0FFF = 0x0FFF;
    
    private static final int MASK_VALUE_F000 = 0xF000;
    
    private static final int TWO_BYTE_OPERAND = 0xFFFF;
    
    private static final int MASK_VALUE_F = 0xF;
    
    private static final int MASK_VALUE_F0 = 0xF0;
    
    private static final int BIT_SHIFT_LEFT_12 = 12;
    
    private static final int BIT_SHIFT_RIGHT_12 = 12;
    
    private static final int MUPLITE_FACTOR_2 = 2;
    
    private static final int DIVIDE_FACTOR_2 = 2;
    
    private static final double BASE_VALUE = 10.0D;
    
    /**
     * The SFloat value in integer type.
     */
    private transient SafetyNumber<Integer> mValue = new SafetyNumber<Integer>();

    /**
     * Convert SFloat from byte to double.
     * 
     * @param byte1 [in]: is the first byte received in byte stream.(lsb of the
     *            short)
     * @param byte2 [in]: is the next byte in the byte stream(msb of the short)
     */
    public SFloat(final byte byte1, final byte byte2)
    {
        short value = (short) (byte2 << ByteConverter.BITS_OF_BYTE);
        value = (short) (value | (byte1 & CommsConstant.TO_POSITIVE));

        mValue.set((int) value, (int) -value);
    }

    /**
     * Convert SFloat from double to byte.
     * 
     * @param value [in]: The double value.
     * @param nPrecision [in]: The precision of floating point.
     */
    public SFloat(final SafetyFloat value, final int nPrecision)
    {
        final SafetyNumber<Integer> first = floatToSafeFloat(value.get(), nPrecision);
        final SafetyNumber<Integer> second = floatToSafeFloat(value.getDiverse(),
                nPrecision);

        if (first.get().equals(second.get()))
        {
            mValue = first;
        }
    }
    
    /**
     * Convert SFloat from int to byte.
     * 
     * @param value [in]: The int value.
     * @param nPrecision [in]: The precision of floating point.
     */
    public SFloat(final SafetyChannel<Integer> value, final int nPrecision)
    {
        final int iValue = SafetyCoding.getOriginValue(value.getValueCH1(), 
                                                value.getValueCH2()); 
        
        mValue = intToSFloat(iValue, nPrecision);   
    }
    
    /**
     * Get the SFloat in short type.
     * 
     * @return short [out]: The SFloat value in short type. Null if no value.
     */
    public final SafetyNumber<Integer> getValue()
    {
        return mValue;
    }

    /**
     * Check that whether this SFloat is NaN or not.
     * 
     * @return SafetyBoolean [out]: True if the value of this SFloat is NaN;
     *         otherwise, false.
     */
    public final SafetyBoolean isNaN()
    {
        SafetyBoolean isNANValue = SafetyBoolean.FALSE;

        if (CommsConstant.NAN == mValue.get().shortValue())
        {
            isNANValue = SafetyBoolean.TRUE;
        }

        return isNANValue;
    }

    /**
     * Convert float value to SFloat value by using int value & precision.
     * 
     * @param fValue [in]: Value in floating point type.
     * @param nPrecision [in]: The precision of result.
     * @return SafetyNumber<Short> [out]: The converted result.
     */
    private SafetyNumber<Integer> intToSFloat(final int iValue, final int nPrecision)
    {
        final SafetyNumber<Integer> result = new SafetyNumber<Integer>();

        // Translate the value from signed to unsigned
        final int nExponent = -nPrecision & MASK_VALUE_000F;
        
        int nMantissa = iValue;
        short nByteValue = -1;
        int iByteValue = -1;

        // Translate the value from signed to unsigned
        nMantissa &= MASK_VALUE_0FFF;
        // Use exponent and mantissa to make up a SFloat
        nByteValue = (short) ((nExponent << BIT_SHIFT_LEFT_12) | nMantissa);
        
        iByteValue = nByteValue & TWO_BYTE_OPERAND;

        result.set(iByteValue, -iByteValue);

        return result;
    }    

    /**
     * Convert float value to SFloat value by using floating point value.
     * 
     * @param fValue [in]: Value in floating point type.
     * @param nPrecision [in]: The precision of result.
     * @return SafetyNumber<Short> [out]: The converted result.
     */
    private SafetyNumber<Integer> floatToSafeFloat(final float fValue, final int nPrecision)
    {
        final SafetyNumber<Integer> result = new SafetyNumber<Integer>();

        // Translate the value from signed to unsigned
        final int nExponent = -nPrecision & MASK_VALUE_000F;
        final double fTempMantissa = fValue * Math.pow(10.0D, nPrecision);
        int nMantissa = 0;
        short nByteValue = -1;

        if (fTempMantissa < 0)
        {
            nMantissa = (int) Math.ceil(fTempMantissa);
        }
        else
        {
            nMantissa = (int) Math.floor(fTempMantissa);
        }

        // Translate the value from signed to unsigned
        nMantissa &= MASK_VALUE_0FFF;
        // Use exponent and mantissa to make up a SFloat
        nByteValue = (short) ((nExponent << BIT_SHIFT_LEFT_12) | nMantissa);

        result.set((int) nByteValue, (int) -nByteValue);

        return result;
    }

    /**
     * Convert float value to SFloat value by using string value with multiply
     * 2.
     * 
     * @param value [in]: Value in floating point type.
     * @param nPrecision [in]: The precision of result.
     * @return SafetyNumber<Short> [out]: The converted result.
     */
    private SafetyNumber<Integer> floatToSafeFloat(final String value, final int nPrecision)
    {
        final SafetyNumber<Integer> result = new SafetyNumber<Integer>();

        // Translate the value from signed to unsigned
        final int nExponent = -nPrecision & MASK_VALUE_000F;
        double fTempMantissa = Float.valueOf(value) * Math.pow(BASE_VALUE, nPrecision);
        int nMantissa = 0;
        short nByteValue = -1;

        // Multiply 2 and divide 2 for different calculation with another
        // method.
        fTempMantissa *= MUPLITE_FACTOR_2;

        if (fTempMantissa < 0)
        {
            nMantissa = (int) Math.ceil(fTempMantissa);
        }
        else
        {
            nMantissa = (int) Math.floor(fTempMantissa);
        }

        // Multiply 2 and divide 2 for different calculation by another method.
        nMantissa /= DIVIDE_FACTOR_2;

        // Translate the value from signed to unsigned
        nMantissa &= MASK_VALUE_0FFF;
        // Use exponent and mantissa to make up a SFloat
        nByteValue = (short) ((nExponent << BIT_SHIFT_LEFT_12) | nMantissa);

        result.set((int) nByteValue, (int) -nByteValue);

        return result;
    }

    /**
     * 
     * @return returns the exponent
     */
    public final SafetyNumber<Byte> getExponent()
    {
        final SafetyNumber<Byte> result = new SafetyNumber<Byte>();
        byte nReturnValue = 0;
        final short nFloatValue = getValue().get().shortValue();

        if (nFloatValue < 0)
        {
            // Translate the value from signed to unsigned
            nReturnValue = (byte) (nFloatValue >> BIT_SHIFT_RIGHT_12 & MASK_VALUE_F | MASK_VALUE_F0);
        }
        else
        {
            // Translate the value from signed to unsigned
            nReturnValue = (byte) (nFloatValue >> BIT_SHIFT_RIGHT_12 & MASK_VALUE_F);
        }

        result.set(nReturnValue, (byte) -nReturnValue);

        return result;
    }

    /**
     * 
     * @return short [out]: The mantissa of this SFloat value.
     */
    public final SafetyNumber<Short> getMantissa()
    {
        final int mask12BIT = 0x800;
        final SafetyNumber<Short> result = new SafetyNumber<Short>();
        short mantissa = 0;
        final short sfloatValue = getValue().get().shortValue();

        if ((sfloatValue & mask12BIT) != 0)
        {
            // Translate the value from signed to unsigned
            mantissa = (short) (sfloatValue & MASK_VALUE_0FFF | MASK_VALUE_F000);
        }
        else
        {
            // Translate the value from signed to unsigned
            mantissa = (short) (sfloatValue & MASK_VALUE_0FFF);
        }

        result.set(mantissa, (short) -mantissa);

        return result;
    }

    /**
     * 
     * @return return the double value.
     */
    public final SafetyFloat toDouble()
    {
        final SafetyFloat result = new SafetyFloat();

        String mantissa = "";
        String nagative = "";
        String number = "";
        String decimal = "";
        final int nExponent = Math.abs(getExponent().get());
        final int nMantissaShort = getMantissa().get();

        // Convert the mantissa from short to String.
        if (nMantissaShort < 0)
        {
            mantissa = String.valueOf(Math.abs(nMantissaShort));
            nagative = "-";
        }
        else
        {
            mantissa = String.valueOf(nMantissaShort);
        }

        // Put the decimal point into the String.
        if (mantissa.length() < nExponent)
        {
            final int nZeroLength = nExponent - mantissa.length();
            
            number = "0";
            decimal = mantissa;

            // Add zero if the number is smaller than one.
            for (int i = 0; i < nZeroLength; i++)
            {
                decimal = "0".concat(decimal);
            }
        }
        else
        {
            final int nPointIndex = mantissa.length() - nExponent;
            
            if (nPointIndex == 0)

            {
                number = "0";
            }
            else
            {
                number = mantissa.substring(0, nPointIndex);
            }

            decimal = mantissa.substring(nPointIndex);
        }

        final String value = nagative.concat(number).concat(".").concat(decimal);
        final float floatValue = Float.valueOf(value);
        
        result.set(floatValue, String.valueOf(floatValue));

        return result;
    }
}
/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Restore the sources to revision 3682.
// [JIRA-ID]:N/A
// [Comment]:Add comment
// (4501 2014-02-13 12:11:31Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-210, ATOS-290, ATOS-291
// [Comment]: Modify for new CISD
// (7067 2014-04-02 06:16:33Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply Coding Guideline
// (8529 2014-04-24 09:24:07Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (10844 2014-06-10 05:16:24Z PhoenixCheng)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-555
// [Comment]: Implements the Fix Point method to replace the floating point
// calculation and Safety for the bG, Insulin, Carbs data processing.
// (10890 2014-06-10 12:41:03Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-557
// [Comment]: Add CRC check in the data receive function
// (11390 2014-06-23 02:43:20Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (12285 2014-07-02 10:29:28Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-272
// [Comment]: Update the footer comment block format.
// (14457 2014-07-31 03:17:21Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: AA-556
// [Comment]: Change BLE communication flow
// (15172 2014-08-11 11:04:37Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (15210 2014-08-12 05:18:44Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (16345 2014-08-21 07:58:29Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (17153 2014-08-27 08:33:50Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-601
// [Comment]: Add CGM Feature check function for recognizing whether the device
// is correct or not.
// (18088 2014-09-02 10:56:38Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (18747 2014-09-11 10:52:20Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (18912 2014-09-12 11:26:50Z kevenwu)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (18912 2014-09-12 11:26:50Z kevenwu)
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
// [Comment]: Comply coding guideline - Fix rule R17, R88, R100, R102, etc.
// (21594 2014-10-13 00:03:06Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-276
// [Comment]: Fix Klocwork issues.
// (R20549 2015-10-01 09:41:00 DWYang)
// ----------------------------------------------------------------------------
// [New Feature] Basal Delivery comms functions
// (R24137 2015-11-13 07:00:18 JacksonHuang)
// ----------------------------------------------------------------------------
// ¦s¨ú³Q©Ú¡C
