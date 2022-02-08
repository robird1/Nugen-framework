/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: UIHelper
 * Brief: Provide the interface function of the UIHelper UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: UIHelper.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Locale;

public class UIHelper
{

    /**
     * 
     * Function Description
     *
     * @param tv
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static private void setDots(TextView tv)
    {

        if (GlobalTools.ReplaceLenghtWithDots)
        {
            tv.setEllipsize(TextUtils.TruncateAt.END);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param textId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setText(ViewGroup group, int viewId, int textId)
    {
        // Added by Henry Tso.
        String contentDescription = "";

        TextView tv1 = (TextView) group.findViewById(viewId);

        tv1.setText(textId);
        setDots(tv1);
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param textId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setText(ViewGroup group, int viewId, double textId)
    {
        TextView tv1 = (TextView) group.findViewById(viewId);
        tv1.setText(formatDoubleString(textId));
        setDots(tv1);
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param text
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setText(ViewGroup group, int viewId, String text)
    {
        TextView tv1 = (TextView) group.findViewById(viewId);
        tv1.setText(text);
        setDots(tv1);
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param imageId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setImage(ViewGroup group, int viewId, int imageId)
    {

        // Added by Henry Tso. To support UIAutomator
        // Implemented only for slide button.
        final String BUTTON_ON = "on";
        final String BUTTON_OFF = "off";

        ImageView iv = (ImageView) group.findViewById(viewId);
        recycleBackgroundBitMap(iv);
        iv.setImageResource(imageId);

        // Added by Henry Tso. To support UIAutomator
        // Implemented only for slide button.
        if (R.drawable.on_slider == imageId)
        {
            iv.setContentDescription(BUTTON_ON);
        }
        else if (R.drawable.off_slider == imageId)
        {
            iv.setContentDescription(BUTTON_OFF);
        }
        else
        {
            // Do nothing for other drawable resource Id at here.
        }
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param imageId
     * @param contentDescription
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setImage(ViewGroup group, int viewId, int imageId,
            String contentDescription)
    {
        ImageView iv = (ImageView) group.findViewById(viewId);
        recycleBackgroundBitMap(iv);
        iv.setImageResource(imageId);

        // If "contentDescription" not empty then set it to content description
        // file of the view
        setContentDescription(iv, contentDescription);
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @param viewId
     * @param imageId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    static public void setImage(ViewGroup group, int viewId, Bitmap imageId)
    {
        ImageView iv = (ImageView) group.findViewById(viewId);
        recycleBackgroundBitMap(iv);
        iv.setImageBitmap(imageId);
    }

    /**
     * 
     * Function Description
     *
     * @param tr
     * @param viewId
     * @param text
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static void setText(View tr, int viewId, String text)
    {
        TextView tv1 = (TextView) tr.findViewById(viewId);
        tv1.setText(text);
        setDots(tv1);
    }

    /**
     * 
     * Function Description
     *
     * @param tr
     * @param viewId
     * @param text
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static void setText(View tr, int viewId, int text)
    {
        TextView tv1 = (TextView) tr.findViewById(viewId);
        tv1.setText(text);
        setDots(tv1);
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return
     * @return String [out] Delete pre line return if exist. Parameter Description
     */
    public static String formatDoubleString(double value)
    {
        String tmp = String.format(Locale.getDefault(), "%.2f", value);
        return tmp;
    }

    public static double formatStringDouble(String value)
    {
        NumberFormat format = NumberFormat.getInstance(Locale.getDefault());
        double d = 0.0;
        
        try
        {
            Number number = format.parse(value);
            d = number.doubleValue();
        }
        catch (ParseException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }

        return d;
    }

    /**
     * Set text API with Content Description to support UIAutomator
     * Added by Henry Tso. To support UIAutomator
     * 
     * @param group [in] ViewGroup object of the UI Component
     *            Range: Valid ViewGroup object
     *            Unit: ViewGroup
     *            Scaling: 1
     * @param viewId [in] View Id of the TextView
     *            Range: -2^31 to (2^31)-1 (Valid resource Id)
     *            Unit: int
     *            Scaling: 1
     * @param textId [in] Text resource Id of the TextView
     *            Range: -2^31 to (2^31)-1 (Valid resource Id)
     *            Unit: int
     *            Scaling: 1
     * @param sDescription [in] Content description for describing the field
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * @return None
     */
    static public void setText(ViewGroup group, int viewId, int textId,
            String sDescription)
    {
        TextView tv1 = (TextView) group.findViewById(viewId);
        tv1.setContentDescription(sDescription);
        tv1.setText(textId);
        setDots(tv1);
    }

    /**
     * Set text API with Content Description to support UIAutomator
     * Added by Henry Tso. To support UIAutomator
     * 
     * @param group [in] ViewGroup object of the UI Component
     *            Range: Valid ViewGroup object
     *            Unit: ViewGroup
     *            Scaling: 1
     * @param viewId [in] View Id of the TextView
     *            Range: -2^31 to (2^31)-1 (Valid resource Id)
     *            Unit: int
     *            Scaling: 1
     * @param text [in] Text for the TextView
     *            Range: Valid String Object
     *            Unit: String
     *            Scaling: 1
     * @param sDescription [in] Content description for describing the field
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * @return None
     */
    static public void setText(ViewGroup group, int viewId, String text,
            String sDescription)
    {
        TextView tv1 = (TextView) group.findViewById(viewId);
        // Added by Henry Tso. To support UIAutomator
        tv1.setContentDescription(sDescription);
        tv1.setText(text);
        setDots(tv1);
    }

    /**
     * Given the View Id, Text resource Id for setting the Content in View.
     * 
     * @param tr [in] Parent View object
     *            Range: Valid View object
     *            Unit: View
     *            Scaling: 1
     * @param viewId [in] Resource Id of the Child view
     *            Range: -2^31 to (2^31)-1
     *            Unit: Int
     *            Scaling: 1
     * @param text [in] Resource Id of the Text for displaying in View
     *            Range: -2^31 to (2^31)-1
     *            Unit: Int
     *            Scaling: 1
     * @param sDescription [in] Content description string for UIAutomator
     *            Range: Valid String object
     *            Unit: String
     *            Scaling: 1
     * @return None
     */
    public static void setText(View tr, int viewId, int text,
            String sDescription)
    {
        TextView tv1 = (TextView) tr.findViewById(viewId);
        // Added by Henry Tso. To support UIAutomator
        tv1.setContentDescription(sDescription);
        tv1.setText(text);
        setDots(tv1);
    }

    /**
     * Call this method to check the given string is not empty. And then
     * put it into Content Description field of the given View object
     * Added by Henry Tso. To support UIAutomator
     * 
     * @param view [in] View object for setting the Content Description
     *            Range: Valid View object
     *            Unit: View
     *            Scaling: 1
     * 
     * @param description [in] String object for setting the Content
     *            Description.
     *            Range: Non-null object. Empty or Non-empty string both allow.
     *            Unit: String
     *            Scaling: 1
     * 
     * 
     * @return void [out] None
     */
    private static void setContentDescription(View view, String description)
    {
        if (!description.isEmpty())
        {
            view.setContentDescription(description);
        }
    }
    
    /**
     * 
     * Function Description
     *
     * @param view
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private static void recycleBackgroundBitMap(ImageView view)
    {
        if (view != null)
        {
            Drawable drawable = view.getBackground();
            if (drawable instanceof BitmapDrawable)
            {
                BitmapDrawable bd = (BitmapDrawable) drawable;
                rceycleBitmapDrawable(bd);
            }
        }
    }

    /**
     * 
     * Function Description
     *
     * @param bd
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private static void rceycleBitmapDrawable(BitmapDrawable bd)
    {
        if (bd != null)
        {
            Bitmap bitmap = bd.getBitmap();
            rceycleBitmap(bitmap);
        }
        bd = null;
    }

    /**
     * 
     * Function Description
     *
     * @param bitmap
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private static void rceycleBitmap(Bitmap bitmap)
    {
        boolean recycled = bitmap.isRecycled();
        if ((bitmap != null) && !recycled)
        {
            bitmap.recycle();
            bitmap = null;
        }
    }
    
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [memory leak issue] update code
// [memory leak issue] update code
