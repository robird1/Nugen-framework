/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SafetyCodings
 * Brief: This class is used for those data requires safety coding before accessing such as basal rate, bG test and so on.
 *
 * Create Date: 11/12/2015
 * $Revision: 24200 $
 * $Author: StanleySu $
 * $Id: SafetyCoding.java 24200 2015-11-16 10:13:22Z StanleySu $
 */

package com.accu_chek.solo_m.rcapp.application.safety;

import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;

import java.math.BigDecimal;

public final class SafetyCoding
{
    
    private static final int CH1_ENCODE_PA = 1;
    
    private static final int CH1_ENCODE_PB = 0;
    
    private static final int CH2_ENCODE_PA = -2;
    
    private static final int CH2_ENCODE_PB = 1;
    
    private static final long CHANNEL_SUM = -1L;
    
    /**
     * The default constructors with no argument required.
     */
    private SafetyCoding()
    {
        // Do nothing for the no argument constructor
    }

    /**
     * Encode channel 1 value.
     * 
     * param original: the original value of channel 1
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return int: return the encoded value of channel 1
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     */
    public static long encodeCH1Value(final long originValue)
    {
        long encodedValue = 0L;
        encodedValue = originValue * CH1_ENCODE_PA + CH1_ENCODE_PB;

        return encodedValue;
    }

    /**
     * Encode channel 1 value.
     * 
     * param original: the original value of channel 1
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return int: return the encoded value of channel 1
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int encodeCH1Value(final int originValue)
    {
        final long value = encodeCH1Value((long) originValue);

        final int channelValue = new BigDecimal(value).intValueExact();

        return channelValue;
    }
    
    /**
     * Encode channel 2 value.
     * 
     * param original: the original value of channel 2
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return long: return the encoded value of channel 2
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     */
    public static long encodeCH2Value(final long originValue)
    {
        long encodedValue = 0L;
        encodedValue = originValue * CH2_ENCODE_PA - CH2_ENCODE_PB;

        return encodedValue;
    }

    /**
     * Encode channel 2 value.
     * 
     * param original: the original value of channel 2
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return int: return the encoded value of channel 2
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int encodeCH2Value(final int originValue)
    {
        final long value = encodeCH2Value((long) originValue);

        final int channelValue = new BigDecimal(value).intValueExact();

        return channelValue;
    }
    
    /**
     * Decode channel 1 value.
     * 
     * param channelValue: channel 1 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return long: return the original value of channel 1
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     */
    public static long decodeCH1Value(final long channelValue)
    {
        long decodedValue = 0L;
        decodedValue = (channelValue - CH1_ENCODE_PB) / CH1_ENCODE_PA;

        return decodedValue;
    }
    
    /**
     * Decode channel 1 value.
     * 
     * param channelValue: channel 1 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return int: return the original value of channel 1
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int decodeCH1Value(final int channelValue)
    {
        final long value = decodeCH1Value((long) channelValue);

        final int originValue = new BigDecimal(value).intValueExact();

        return originValue;
    }

    /**
     * Decode channel 2 value.
     * 
     * param channelValue: channel 2 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return long: return the original value of channel 2
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     */
    public static long decodeCH2Value(final long channelValue)
    {
        long decodedValue = 0L;
        decodedValue = (channelValue + CH2_ENCODE_PB) / CH2_ENCODE_PA;

        return decodedValue;
    }

    /**
     * Decode channel 2 value.
     * 
     * param channelValue: channel 2 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return int: return the original value of channel 2
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int decodeCH2Value(final int channelValue)
    {
        final long value = decodeCH2Value((long) channelValue);

        final int originValue = new BigDecimal(value).intValueExact();

        return originValue;
    }

    /**
     * Return true if the safety comparison of the two channel values is no
     * problem.
     * 
     * param value1: channel 1 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * param value2: channel 2 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return SafetyBoolean: return true if the 2 channel comparison is OK.
     * Range: SafetyBoolean.TRUE / SafetyBoolean.FALSE
     * Unit: SafetyBoolean
     * Scaling: 1
     */
    public static SafetyBoolean compareTwoChannel(final long value1, final long value2)
    {
        SafetyBoolean result = SafetyBoolean.FALSE;

        final long sum = value1 * 2 + value2;

        if (sum == CHANNEL_SUM)
        {
            result = SafetyBoolean.TRUE;
        }
        else
        {
            result = SafetyBoolean.FALSE;
        }

        return result;
    }

    /**
     * Compare the 2 channel values and return the original value if the
     * comparison is OK. Throw DataIntegrityException if the comparison is
     * failed.
     * 
     * param valueCH1 : channel 1 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * param valueCH2 : channel 2 value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return long : the original value
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * @throw DataIntegrityException: throw this exception if the 2 channel
     *        comparison is failed.
     * 
     */
    public static long getOriginValue(final long valueCH1, final long valueCH2)
    {
        long value = 0;
        final SafetyBoolean isResultOk = compareTwoChannel(valueCH1, valueCH2);

        if (isResultOk.getByte() == SafetyBoolean.TRUE.getByte())
        {
            value = decodeCH1Value(valueCH1);
        }
        else
        {
            throw new DataIntegrityException();
        }

        return value;
    }

    /**
     * Compare the 2 channel values and return the original value if the
     * comparison is OK. Throw DataIntegrityException if the comparison is
     * failed.
     * 
     * param valueCH1 : channel 1 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * param valueCH2 : channel 2 value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return int : the original value
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     */
    public static int getOriginValue(final int valueCH1, final int valueCH2)
    {
        final long value = getOriginValue((long) valueCH1, (long) valueCH2);

        final int oriValue = new BigDecimal(value).intValueExact();

        return oriValue;
    }

    /**
     * Retrieve SafetyChannel object with the given value which has not been 
     * encoded by safety.
     * 
     * param oriValue: the original value before safety coding
     * Range: the range which can be represented by a long value
     * Unit: long
     * Scaling: 1
     * 
     * return SafetyChannel: SafetyChannel object for Long type
     * Range:
     * Unit:
     * Scaling:
     */
    public static SafetyChannel<Long> getSafetyChannel(final long originValue)
    {
        final long value1 = encodeCH1Value(originValue);
        final long value2 = encodeCH2Value(originValue);
        final SafetyChannel<Long> channel = new SafetyChannel<Long>(value1, value2);

        return channel;
    }

    /**
     * Retrieve SafetyChannel object with the given value which has not been 
     * encoded by safety.
     * 
     * param oriValue: the original value before safety coding
     * Range: the range which can be represented by a int value
     * Unit: int
     * Scaling: 1
     * 
     * return SafetyChannel: SafetyChannel object for Integer type
     * Range:
     * Unit:
     * Scaling:
     */
    public static SafetyChannel<Integer> getSafetyChannel(final int originValue)
    {
        final int value1 = encodeCH1Value(originValue);
        final int value2 = encodeCH2Value(originValue);
        final SafetyChannel<Integer> channel = new SafetyChannel<Integer>(value1,
                value2);

        return channel;
    }
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
