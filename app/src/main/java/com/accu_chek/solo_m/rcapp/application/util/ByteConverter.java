package com.accu_chek.solo_m.rcapp.application.util;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
/*
 * The ByteConverter converts various data types into byte array and supports with byte order.
 * 
 */
public class ByteConverter
{

    /**
     *Bit number in a byte. 
     */
    public static final int BITS_OF_BYTE = 8;
    /*
     * bit mask
     */
    public static final int MASK_BIT = 0xff;
    /*
     * length of allocated byte buffer at converting short
     */
    public static final int INT_2 = 2;
    /*
     * length of allocated byte buffer at converting int
     */
    public static final int INT_4 = 4;
    /*
     * length of allocated byte buffer at converting flag
     */
    public static final int INT_24 = 24;

    /**
     * Flags for bit mask
     * 
     */
    public static final class FlagBit
    {
        public static final int BIT0 = 1;
        public static final int BIT1 = 2;
        public static final int BIT2 = 4;
        public static final int BIT3 = 8;
        public static final int BIT4 = 16;
        public static final int BIT5 = 32;
        public static final int BIT6 = 64;
        public static final int BIT7 = 128;
        public static final int BIT8 = 256;
        public static final int BIT9 = 512;
        public static final int BIT10 = 1024;
        public static final int BIT11 = 2048;
        public static final int BIT12 = 4096;
        public static final int BIT13 = 8192;
        public static final int BIT14 = 16384;
        public static final int BIT15 = 32768;

    }

    /**
     * 
     * The function converts an int into byte array.
     *
     * param data The int to be converted.
     * Range: valid int value.
     * Unit: int
     * Scaling:1
     * return byte[] [out] The byte array of the converted int.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(int data)
    {
        return new byte[] { (byte) ((data >> 0) & MASK_BIT),
                (byte) ((data >> FlagBit.BIT3) & MASK_BIT),
                (byte) ((data >> FlagBit.BIT4) & MASK_BIT),
                (byte) ((data >> INT_24) & MASK_BIT), };
    }
    /**
     * 
     * The function converts a short into byte array.
     *
     * param data The short to be converted.
     * Range: valid short value.
     * Unit: short
     * Scaling:1
     * return byte[] [out] The byte array of the converted short.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(short data)
    {
        return new byte[] { (byte) ((data >> 0) & MASK_BIT),
                (byte) ((data >> FlagBit.BIT3) & MASK_BIT), };
    }
    /**
     * 
     * The function converts a byte into byte array.
     *
     * param data The byte to be converted.
     * Range: valid byte value.
     * Unit: a byte
     * Scaling:1
     * return byte[] [out] The byte array of the converted byte.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(byte data)
    {
        return new byte[] { data };
    }
    /**
     * 
     * The function converts a float into byte array.
     *
     * param data The float to be converted.
     * Range: valid float value.
     * Unit: a float
     * Scaling:1
     * return byte[] [out] The byte array of the converted float.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(float data)
    {
        return getBytes(Float.floatToRawIntBits(data));
    }
    /**
     * 
     * The function converts an int into byte array with specified byte order.
     *
     * param data The int to be converted.
     * Range: valid int value.
     * Unit: int
     * Scaling:1
     * param order The specified byte order for converting
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return byte[] [out] The byte array of the converted int.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(int data, ByteOrder order)
    {

        ByteBuffer buf = ByteBuffer.allocate(INT_4);
        buf.putInt(data);
        buf.order(order);

        return buf.array();
    }
    /**
     * 
     * The function converts a short into byte array with specified byte order.
     *
     * param data The short to be converted.
     * Range: valid short value.
     * Unit: short
     * Scaling:1
     * param order The specified byte order for converting
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return byte[] [out] The byte array of the converted short.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(short data, ByteOrder order)
    {

        ByteBuffer buf = ByteBuffer.allocate(INT_2);

        buf.putShort(data);
        buf.order(order);

        return buf.array();
    }
    /**
     * 
     * The function converts a float into byte array with specified byte order.
     *
     * param data The float to be converted.
     * Range: valid float value.
     * Unit: float
     * Scaling:1
     * param order The specified byte order for converting
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return byte[] [out] The byte array of the converted float.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] getBytes(float data, ByteOrder order)
    {

        return getBytes(Float.floatToRawIntBits(data), order);
    }
    /*
     *The function combines byte[] of an ArrayList<byte[]> and builds a byte[].
     *
     * param members The ArrayList<byte[]> which contains a list of byte[]
     * Range: Valid ArrayList<byte[]> object.
     * Unit: N/A
     * Scaling: 1
     * return byte[] [out] The byte array built.
     * Range: Not NULL byte array.
     * Unit: byte[]
     * Scaling:1 
     */
    public static byte[] buildByteArray(ArrayList<byte[]> members)
    {

        byte[] array = null;
        int size = 0;

        for (byte[] b : members)
        {
            size += b.length;
        }

        array = new byte[size];
        // LogUtilities.dLog(TAG, "Size of byte array=" + size);

        // String addMe = "";
        int index = 0;
        for (byte[] arr : members)
        {
            for (int j = 0; j < arr.length; j++)
            {
                byte b = arr[j];
                // addMe += String.format("%x", b) + " ";
                array[index + j] = b;
            }
            index += arr.length;
        }

        // LogUtilities.dLog(TAG, "Byte array=" + addMe);
        return array;

    }
    /**
     * 
     * The function retrieves two bytes from a ByteBuffer with specified byte order and converts them into a short.
     *
     * param b The ByteBuffer with source data.
     * Range: A valid ByteBuffer
     * Unit: ByteBuffer
     * Scaling: 1
     * param order The specified byte order at retrieving bytes from ByteBuffer
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return short [out] A short value
     * Range: Valid short. The user application should take care of the affect of the signed bit.
     * Unit:N/A
     * Scaling:1
     */
    public static short readShort(ByteBuffer b, ByteOrder order)
    {
        byte[] buf = new byte[] { (byte) (b.get() & MASK_BIT),
                (byte) (b.get() & MASK_BIT) };

        return ByteBuffer.wrap(buf).order(order).getShort();

    }
    /**
     * 
     * The function retrieves four bytes from a ByteBuffer with specified byte order and converts them into an int.
     *
     * param b The ByteBuffer with source data.
     * Range: A valid ByteBuffer
     * Unit: ByteBuffer
     * Scaling: 1
     * param order The specified byte order at retrieving bytes from ByteBuffer
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return int [out] An int value
     * Range: Valid int. The user application should take care of the affect of the signed bit.
     * Unit:N/A
     * Scaling:1
     */
    public static int readInt(ByteBuffer b, ByteOrder order)
    {
        byte[] buf = new byte[] { (byte) (b.get() & MASK_BIT),
                (byte) (b.get() & MASK_BIT), (byte) (b.get() & MASK_BIT),
                (byte) (b.get() & MASK_BIT) };

        return ByteBuffer.wrap(buf).order(order).getInt();

    }
    /**
     * 
     * The function retrieves bytes from a ByteBuffer with specified byte order and converts them into a byte array.
     *
     * param b The ByteBuffer with source data.
     * Range: A valid ByteBuffer
     * Unit: ByteBuffer
     * Scaling: 1
     * param order The specified byte order at retrieving bytes from ByteBuffer
     * Range: ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN
     * Unit: N/A
     * Scaling: 1
     * return short [out] A short value
     * Range: byte array.
     * Unit:N/A
     * Scaling:1
     */
    public static byte[] readBytes(ByteBuffer b, ByteOrder order, int length)
    {
        byte[] buf = new byte[length];

        b.get(buf, 0, length);
        ByteBuffer.wrap(buf).order(order).array();

        return buf;
    }
}