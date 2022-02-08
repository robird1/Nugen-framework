/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LAD0002d
 * Brief: Provide the function of the LAD0002d layout
 *
 * Create Date: 01/22/2015
 * $Revision: 24175 $
 * $Author: AdamChen $
 * $Id: LAD0002d.java 24175 2015-11-16 09:33:09Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts;

import android.app.Activity;
import android.view.View;
import android.view.ViewGroup;

import com.accu_chek.solo_m.rcapp.application.gui.interfaces.IFooterButton;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.UIHelper;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

public class LAD0002d extends WarningLayoutBase
{

    private static final int LAYOUT_ID = R.layout.lad0002d;

    /**
     * 
     * @param activity
     */
    public LAD0002d(Activity activity)
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
     * Setup layout using specified parameters
     * 
     * @param titleId
     *            Id for prompt title string
     * @param errorNumber
     *            error number
     * @param errorText
     *            error text
     */
    public void setupError(int titleId, int errorText)
    {

        setup(R.drawable.error_75x75, titleId, errorText);
    }

    /**
     * Setup layout using specified parameters
     * 
     * @param titleId
     *            Id for prompt title string
     * @param warningNumber
     *            warning number
     * @param warningText
     *            warning text
     */
    public void setupWarning(int titleId, int warningText)
    {

        setup(R.drawable.warning_75x75, titleId, warningText);
    }

    /**
     * Setup layout using specified parameters
     * 
     * @param infoIconId
     *            Id for icon used in the dialog body
     * @param titleId
     *            Id for prompt title string
     * @param errorNumber
     *            error / warning number
     * @param errorText
     *            error / warning text
     */
    public void setup(int infoIconId, int titleId, int errorText)
    {

        ViewGroup container = findContainerById(R.id.warning_relativeLayout1);
        setup(container, infoIconId, titleId, errorText);
    }

    /**
     * 
     * Function Description
     *
     * @param container
     * @param infoIconId
     * @param infoTitleId
     * @param errorText
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setup(ViewGroup container, int infoIconId, int infoTitleId,
            int errorText)
    {
        UIHelper.setImage(container, R.id.warning_img1, infoIconId);
        UIHelper.setText(container, R.id.warning_title_text, infoTitleId);
        UIHelper.setText(container, R.id.warning_text, errorText);
    }

    // modify by Steve {
    /**
     * Setup Lower Action Bar 1
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonTextId, IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002d_lower_ab, buttonTextId, button);
    }

    /**
     * Setup Lower Action Bar 2
     * 
     * @return Action Bar View
     */
    public View setupLowerActionBar(int buttonText1Id, int buttonText2Id,
            IFooterButton button)
    {
        return addLowerActionBar(R.id.lad002d_lower_ab, buttonText1Id,
                buttonText2Id, button);
    }
    // } modify by Steve

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */