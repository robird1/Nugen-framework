/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0026
 * Brief: Provide the function of the LAD0026 layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24929 $
 * $Author: AdamChen $
 * $Id: LAD0026.java 24929 2015-11-26 09:01:08Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import android.app.Activity;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0026 extends LayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0026;

    /**
     * 
     * @param activity
     */
    public LAD0026(Activity activity)
    {
        super(activity);
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getLayoutId()
    {
        return LAYOUT_ID;
    }

    /**
     * 
     * Function Description
     *
     * @param titleId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTitle(int titleId)
    {
        TextView t = (TextView) findViewById(R.id.id_license_title);
        t.setText(titleId);
    }

    /**
     * 
     * Function Description
     *
     * @param rawFileId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setText(int rawFileId)
    {
        InputStream inputStream = getActivity().getResources().openRawResource(
                rawFileId);

        InputStreamReader inputreader = new InputStreamReader(inputStream);
        BufferedReader buffreader = new BufferedReader(inputreader);
        String line;
        StringBuilder text = new StringBuilder();
        text.append("\n");

        try
        {
            line = buffreader.readLine();
            
            while (line != null)
            {
                text.append(line);
                text.append('\n');
                
                line = buffreader.readLine();
            }

        }
        catch (IOException e)
        {
            e.printStackTrace();
            text.append(e.getMessage());
        }
        finally
        {
            // Apply to the coding standard
        }
        
        TextView t = (TextView) findViewById(R.id.id_license_text);
        t.setText(text);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
