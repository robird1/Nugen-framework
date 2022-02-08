package com.accu_chek.solo_m.rcapp.application.util;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import android.util.Log;

public final class Debug
{

    private static final boolean isDebug = true;
    public static final int MASK_BIT = 0xff;

    private Debug() throws Exception
    {

        throw new Exception("");
    }

    public static void printI(String tag, String str)
    {
        if (isDebug)
        {
            Log.i(tag, str);
        }
    }

    public static void printE(String tag, String str)
    {
        if (isDebug)
        {
            Log.e(tag, str);
        }
    }

    public static void printD(String tag, String str)
    {
        if (isDebug)
        {
            Log.d(tag, str);
        }
    }

    public static void dumpPayload(String tag, byte[] b)
    {
        String printBuffer;
        if (isDebug)
        {
            int i, j;
            int len = b.length;
            for (i = 0; i < len; i += 10)
            {
                printBuffer = "";
                for (j = 0; ((j + i) < len) && (j < 10); j++)
                {
                    printBuffer = printBuffer + " 0x";
                    printBuffer = printBuffer + Integer.toHexString(b[i + j] & MASK_BIT);
                }
                // printI(tag,
                // "PL[" + i + "]" + Integer.toHexString(b[i] & MASK_BIT));
                printI(tag, "PL[" + i + "]" + printBuffer);
            }
        }
    }

    public static void dumpPayload(String action, byte b)
    {

//        String fileName = "/data/nugen/bgmlog/BgmCommunicationLog.txt";
//
//        try
//        {
//            Debug.printI("test", "dumpPayload++");
//            FileWriter fos = null;
//            fos = new FileWriter(fileName, true);
//            BufferedWriter out = new BufferedWriter(fos);
//            DateFormat dateFormat = new SimpleDateFormat("yy/MM/dd");
//            // get current date/time
//            String Date = dateFormat.format(Calendar.getInstance().getTime());
//            DateFormat   timeFormat = new SimpleDateFormat("HH:mm:ss.SSS");
//            String time = timeFormat.format(Calendar.getInstance().getTime());
//       
//            if (b == 0x00)
//            {
//                out.write(Date + " " +time+" "+ action + "\n");
//            }
//            else
//            {
//                out.write(Date + " " +time+" "+action + " "
//                        + toHex(b) + "\n");
//            }
//            out.flush();
//            out.close();
//            fos.close();
//            Debug.printI("test", "dumpPayload--");
//        }
//        catch (IOException e)
//        {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//            Debug.printI("test", "IOException");
//        }
//        finally
//        {
//
//        }
    }

    private static String toHex(byte b)
    {
        int shift = 4;
        byte data = 0xf;
        String tohex = "" + "0123456789ABCDEF".charAt(data & b >> shift)
                + "0123456789ABCDEF".charAt(b & data);
        return tohex;
    }

}
