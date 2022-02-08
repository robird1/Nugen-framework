/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SoloMFragmentActivity
 * Brief: The Fragment style of the base activity
 *
 * Create Date: 01/22/2015
 * $Revision: 24932 $
 * $Author: AdamChen $
 * $Id: SoloMFragmentActivity.java 24932 2015-11-26 09:25:38Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.buildingblocks;

import android.annotation.TargetApi;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.widget.FrameLayout;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.NuGenFragmentActivity;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.sothree.slidinguppanel.SlidingUpPanelLayout;
import com.sothree.slidinguppanel.SlidingUpPanelLayout.PanelState;

import java.util.Locale;

public class SoloMFragmentActivity extends NuGenFragmentActivity
{

    //Quick Info instance
    private SlidingUpPanelLayout mSlidingPanelLayout = null;

    /**
     * 
     * Check the right-to-left layout
     * 
     * @return boolean [out]
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    public static boolean isRTL()
    {
        return isRTL(Locale.getDefault());
    }

    /**
     * 
     * Check the right-to-left layout
     * 
     * @param locale [in]
     *            Range: The valid reference of Local
     *            Unit: Local
     *            Scaling: 1
     * 
     * @return boolean [out]
     */
    public static boolean isRTL(Locale locale)
    {
        final int directionality = Character.getDirectionality(locale
                .getDisplayName().charAt(0));
        return directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT
                || directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC;
    }

    /**
     * 
     * Called when the activity is starting.
     * 
     * @param savedInstanceState[in]
     *            Range: The valid reference of Bundle
     *            Unit: Bundle
     *            Scaling: 1
     */
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        // Force to support the RTL layout
        forceRTLIfSupported();
    }

    /**
     * Override default behavior for setting layout in order to replace
     * frame layout with real layout.
     * 
     * @param layoutResID[in]
     *            Range: -2^31 - (2^31)-1
     *            Unit: int
     *            Scaling: 1
     */
    @Override
    public void setContentView(int layoutResID)
    {
        super.setContentView(R.layout.activity_base);
        
        //Obtain frame layout handler by resource id
        FrameLayout frameContent = (FrameLayout) findViewById(R.id.layout_base_frame);
        
        getLayoutInflater().inflate(layoutResID, frameContent);

        //Obtain quick info instance by resource id
        mSlidingPanelLayout = (SlidingUpPanelLayout) findViewById(R.id.sliding_layout);
        mSlidingPanelLayout.setOverlayed(true);
        
        // TODO: if !hasStatusBar, hide it.
    }

    /**
     * 
     * The interface of the enable/disable Sliding function
     * 
     * @param b [in]
     *            Range: true, false
     *            Unit: boolean
     *            Scaling: 1
     * 
     */
    protected void setEnabledSliding(boolean b)
    {
        mSlidingPanelLayout.setEnabled(b);
    }

    /**
     * Override default back key pressed.
     */
    @Override
    public void onBackPressed()
    {
        boolean isCloseSlide = closeSlidingPanelUpDown();
        
        if (isCloseSlide == true)
        {
            // do not finish activity
        }
        else
        {
            super.onBackPressed();
        }
    }

    /**
     * Close drawer up to down if it has been opened.
     * This method has to be invoked in every activity onBackPressed() method if
     * do not use same
     * method from BaseActivity (super.onBackPressed())
     * 
     * @return true - if panel was opened, false - panel was already closed
     *         Range: true, false
     *         Unit: boolean
     *         Scaling: 1
     */
    protected boolean closeSlidingPanelUpDown()
    {
        boolean ret = false;
        
        // Check quick info instance and status
        PanelState panelState = mSlidingPanelLayout.getPanelState();
        
        if ((mSlidingPanelLayout != null)
                && (panelState == PanelState.EXPANDED))
        {
            // Set quick info instance status 
            mSlidingPanelLayout.setPanelState(PanelState.COLLAPSED);
            ret =  true;
        }
        else
        {
            ret =  false;
        }
        
        return ret;
    }
    
    /**
     * 
     * Force to support the right-to-left layout
     *
     */
    @TargetApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
    private void forceRTLIfSupported()
    {
        if (isRTL())
        {
            //Check build version
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
            {
                this.getWindow().getDecorView().setLayoutDirection(
                        View.LAYOUT_DIRECTION_RTL);
            }
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