/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: TopActionBar
 * Brief: Provide the interface function of the TopActionBar UI component
 *
 * Create Date: 10/16/2015
 * $Revision: 24734 $
 * $Author: SteveSu $
 * $Id: TopActionBar.java 24734 2015-11-25 01:31:01Z SteveSu $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.view.View;
import android.view.ViewStub;
import android.widget.ImageView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.security.InvalidParameterException;

public class TopActionBar
{

    // Add by Henry Tso. To support UIAutomator
    private static String mScreenID = "";

    private static ImageView mImg1 = null; // memory leak suspect

    private static ImageView mImg2 = null; // memory leak suspect

    /**
     * Sets up the top action bar 1
     * 
     * @param context
     *            ActionBarActivity instance
     * @param titleId
     *            Title string id
     * @param iconId
     *            Left icon drawable id
     */
    public static void setup(Activity context, int titleId, int iconId)
    {
        setupBaseActionBar(getActionBar(context, R.layout.top_action_bar1),
                titleId, iconId);
    }

    // memory leak suspect {
    // TopActionBar was implemented by Singleton. Therefore, call the
    // releaseImageView
    // will cause the top bar icon disappear from current displayed Activity
    public static void releaseImageView(ImageView img)
    {
        // Drawable drawable = null;
        //
        // if (null != img)
        // {
        // drawable = img.getDrawable();
        //
        // if (null != drawable)
        // {
        // drawable.setCallback(null);
        //
        // img.setImageDrawable(null);
        // img.setBackground(null);
        // }
        // }
    }
    // } memory leak suspect
    
    /**
     * Sets up the top action bar 4
     * 
     * @param context
     *            Activity containing an action bar ViewStub with predefined id.
     * @param titleId
     *            Title string id
     * @param iconId
     *            Left icon drawable id
     * @param text1
     *            Text 1 string
     * @param text2
     *            Text 2 string
     */
    public static void setup(Activity context, int titleId, int iconId,
            String text1, String text2)
    {
        // TODO: The "Action Bar 6" is actually Action Bar 4 in the Building
        // Blocks definition
        View mCustomView = getActionBar(context, R.layout.top_action_bar6);
        setupBaseActionBar(mCustomView, titleId, iconId);
        UIHelper.setText(mCustomView, R.id.topaction6_t1, text1);
        UIHelper.setText(mCustomView, R.id.topaction6_t2, text2);
    }

    /**
     * Sets up the top action bar (not-defined in documentation)
     */
    public static void setup(Activity context, int titleId, int iconId,
            String text1)
    {
        View mCustomView = getActionBar(context, R.layout.top_action_bar4);
        setupBaseActionBar(mCustomView, titleId, iconId);
        UIHelper.setText(mCustomView, R.id.topaction4_t1, text1);
    }

    /**
     * Sets up the top action bar 2
     * 
     * @param context
     *            ActionBarActivity instance
     * @param titleId
     *            Title string id
     * @param iconLeftId
     *            Left icon drawable id
     * @param iconRightId
     *            Right icon drawable id
     */
    public static void setup(Activity context, int titleId, int iconLeftId,
            int iconRightId)
    {
        View mCustomView = getActionBar(context, R.layout.top_action_bar2);
        setupBaseActionBar(mCustomView, titleId, iconLeftId);
        releaseImageView(mImg2);
        mImg2 = (ImageView) mCustomView.findViewById(R.id.actionbar_img2);
        mImg2.setImageResource(iconRightId);
    }

    /**
     * Get the view of action bar right icon.
     * 
     * @return The view of action bar right icon.
     *         Range: valid ImageView object
     *         Unit: ImageView
     *         Scaling: 1
     * @see mImg2
     */
    public static ImageView getActionBarRightIcon()
    {
        return mImg2;
    }
    
    // add by Henry {
    /**
     * 
     * Function Description
     *
     * @param sContentDescriptionScreenID
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public static void setScreenId(String sContentDescriptionScreenID)
    {
        String[] className = sContentDescriptionScreenID.split("rcapp.");
        int len = className.length;

        if (len > 0)
        {
            mScreenID = className[len - 1];
        }
        else
        {
            mScreenID = sContentDescriptionScreenID;
        }
    }
    // } add by Henry
    
//    protected static void get
    /**
     * 
     * Function Description
     *
     * @param context
     * @param actionBarLayoutId
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    private static View getActionBar(Activity context, int actionBarLayoutId)
    {
        ViewStub stub = (ViewStub) context.findViewById(R.id.action_bar_stub);
        if (stub == null)
        {
            throw new InvalidParameterException(
                    "The specified context does not contain a ViewStub with the id R.id.action_bar_stub. Check if the layout is already defined (i.e. setContentView was called).");
        }
        stub.setLayoutResource(actionBarLayoutId);
        View mCustomView = stub.inflate();
        return mCustomView;
    }

    /**
     * 
     * Function Description
     *
     * @param customView
     * @param titleId
     * @param iconId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private static void setupBaseActionBar(View customView, int titleId,
            int iconId)
    {
        UIHelper.setText(customView, R.id.actionbar_title_text, titleId,
                mScreenID);
        releaseImageView(mImg1);
        mImg1 = (ImageView) customView.findViewById(R.id.actionbar_img1);
        mImg1.setImageResource(iconId);
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
// [NSIQ-55] Disable "FrontManager" function to solve the memory leak problem.
// (R24683 2015-11-24 01:57:50 AdamChen)
// ----------------------------------------------------------------------------
// [Reminder] fix compilation error
