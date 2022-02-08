package com.accu_chek.solo_m.rcapp.application.fwupdate;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class FWUpdateZip
{
    //for debugging, will be removed in formal release
    static final String TAG = FWUpdate.class.getSimpleName();
    
    String mSrc = null;
    String mDst = null;
    /**
     * 
     */
    private static int mMaxBufLen = 64;
    
    FWUpdateZip(String src, String dst)
    {
        mSrc = src;
        mDst = dst;
    }
    
    public int unzip()
    {
        int err = Error.ERR_OK;
        FileInputStream fip = null;
        BufferedInputStream bis = null;
        ZipInputStream zIn = null;
        
        //prepare input stuff including a zip input tream
        try
        {
            fip = new FileInputStream(mSrc);
            bis = new BufferedInputStream(fip);
            //create a zip input stream 
            zIn = new ZipInputStream (bis);
        }
        catch (FileNotFoundException e1)
        {
            // TODO Auto-generated catch block
            e1.printStackTrace();
            err = Error.ERR_ZIP_EOF;
        }
        
        if(err != Error.ERR_ZIP_EOF)
        {
            try
            {
                decompress(zIn, mDst);
            }
            catch (IOException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
                err = Error.ERR_ZIP_EOF;
            }
        }
        
        return err;
    }
    
    private int decompress(ZipInputStream zin, String outdir) throws IOException
    {
        int err = Error.ERR_OK;
        //get a entry in a zip package
        ZipEntry ze = zin.getNextEntry();
        //prepare the output stuff
        File fileout = new File(outdir+ze.getName());
        FileOutputStream fos = new FileOutputStream(fileout);
        int lr = 0;
        
        //if this entry is not a directory, do the decompression
        if(ze != null)
        {
            lr = copyEntryData(zin, fos);
            fos.close();
            zin.close();
        }
        else
        {
            err = Error.ERR_ZIP_EOF;
        }
        return err;
    }
    
    private int copyEntryData(ZipInputStream zin, FileOutputStream fOut) throws IOException
    {
        byte[] buf = new byte[mMaxBufLen];
        int len = 0;
        int len_total = 0;
        
        //read data of a zip entry to the end
        do
        {
            len = zin.read(buf);
            fOut.write(buf, 0, len);
            len_total += len;
        }
        while(len > 0);
        
        return len_total;
    }
}
