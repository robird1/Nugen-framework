/**
 * ===========================================================================
 * Copyright 2014 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.demo.safety.SafetyBoolean
 * Brief: 
 *
 * Create Date: 1/13/2015
 * $Revision: 24184 $
 * $Author: StanleySu $
 * $Id: SafetyBoolean.java 24184 2015-11-16 09:43:42Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import java.io.Serializable;

public enum SafetyBoolean implements Serializable
{
    TRUE((byte) 0xD4),
    FALSE((byte) 0x2B);

    // the following 2 constants are for the operation of bit comparison
    private static final byte TRUE_INVERSE = (byte) 0x2B;
    private static final byte FALSE_INVERSE = (byte) 0xD4;

    // The byte value of TRUE(0xD4) or FALSE(0x2B)
    private final byte mValue;

    /**
     * Constructs a SafetyBoolean item with byte value.
     * 
     * @param value [in] the byte value of SafetyBoolean item
     */
    private SafetyBoolean(final byte value)
    {
        mValue = value;
    }

    /**
     * Return the byte value of this SafetyBoolean item. <br />
     * 
     * It will and the kept unsigned byte value with TRUE_INVERSE (0x2B) value
     * if the item is TRUE or FALSE_INVERSE (0xD4) value if the item is FALSE.
     * If the and result are not 0, it will throw DataIntegrityException.
     * 
     * @return byte [out] Return the byte value of this SafetyBoolean item.
     */
    public byte getByte()
    {
        switch (this)
        {
        case TRUE :
            if ((mValue & TRUE_INVERSE) != 0)
            {
//                throw new DataIntegrityException();
            }
            break;
        case FALSE :
            if ((mValue & FALSE_INVERSE) != 0)
            {
//                throw new DataIntegrityException();
            }
            break;
        default :
//            throw new DataIntegrityException();
        }
        return mValue;
    }

    /**
     * Return SafetyBoolean.TRUE if the passed in byte value is same as byte
     * value of SafetyBoolean.TRUE. Return SafetyBoolean.FALSE if the passed in
     * byte value is same as byte value of SafetyBoolean.FALSE. Otherwise return
     * null.
     * 
     * @param value [in] the byte value of specified SafetyBoolean item
     * @return SafetyBoolean [out] Returns the enum item with the specified byte
     *         value of the specified SafetyBoolean type.
     */
    public static SafetyBoolean valueOf(final byte value)
    {
        SafetyBoolean isResultOK = null;

        if ((value & TRUE_INVERSE) == 0)
        {
            isResultOK = SafetyBoolean.TRUE;
        }
        else if ((value & FALSE_INVERSE) == 0)
        {
            isResultOK = SafetyBoolean.FALSE;
        }
        else
        {
            // do nothing
        }

        return isResultOK;
    }

    /**
     * Do the and operation with passed in safety booleans. It will throw
     * IllegalArgumentException if one of the passed in parameter is null.
     * Otherwise return the result of and operation.
     * 
     * @param isBooleans [in] The non-null safety booleans to do the and
     *            operation.
     * @return SafetyBoolean [out] the result of safety and operation.
     */
    public SafetyBoolean safetyAnd(final SafetyBoolean... isBooleans)
    {
        SafetyBoolean isResultOK = this;

        if (isBooleans != null)
        {
            int nIndex = 0;
            final int nLength = isBooleans.length;

            while ((nIndex < nLength) 
                    && (isResultOK.getByte() == TRUE.getByte()))
            {
                if (isBooleans[nIndex] == null)
                {
                    throw new IllegalArgumentException("all parameters can't "
                            + "be null");
                }
                else
                {
                    if (isBooleans[nIndex].getByte() == FALSE.getByte())
                    {
                        isResultOK = SafetyBoolean.FALSE;
                    }
                }

                nIndex++;
            }
        }
        else
        {
            throw new IllegalArgumentException("all parameters can't be null");
        }

        return isResultOK;
    }

    /**
     * Do the or operation with passed in safety booleans. It will throw
     * IllegalArgumentException if one of the passed in parameter is null.
     * Otherwise return the result of or operation
     * 
     * @param isBooleans [in] The non-null safety booleans to do the or operation.
     * @return SafetyBoolean [out] the result of safety or operation.
     */
    public SafetyBoolean safetyOr(final SafetyBoolean... isBooleans)
    {
        SafetyBoolean isResultOK = this;

        if (isBooleans != null)
        {
            int nIndex = 0;
            final int nLength = isBooleans.length;

            while ((nIndex < nLength) 
                    && (isResultOK.getByte() == FALSE.getByte()))
            {
                if (isBooleans[nIndex] == null)
                {
                    throw new IllegalArgumentException("all parameters can't "
                            + "be null");
                }
                else
                {
                    if (isBooleans[nIndex].getByte() == TRUE.getByte())
                    {
                        isResultOK = SafetyBoolean.TRUE;
                    }
                }

                nIndex++;
            }
        }
        else
        {
            throw new IllegalArgumentException("all parameters can't be null");
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
