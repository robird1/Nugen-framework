/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: Button_B23
 * Brief: Provide the function of the Button_B23 UI component
 *
 * Create Date: 12/09/2014
 * $Revision: 24683 $
 * $Author: AdamChen $
 * $Id: Button_B23.java 24683 2015-11-24 05:57:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

public class Button_B23 extends ButtonBasic
{
    
    OnClickListener mListener = null;
    
    private B23Container container =  null;
    
    // Added by Henry Tso.
    private ViewGroup mGroup = null;
    
    // Defined for storing the ImageView object of the Meal Time icon
    private ImageView mMealTimeIcon = null;
    
    // Defined for storing the ImageView object of the Health Event icon
    private ImageView mHealthEventIcon = null;
    
    // Defined for storing the ImageView object of the Carbs icon
    private ImageView mCarbsIcon = null;
    
    // Defined for storing the TextView object of the Carbs Value
    private TextView mCarbsValue = null;
    
    // Defined for storing the ImageView object of the Note icon
    private ImageView mNoteIcon = null;

    /**
     * 
     * @param values
     * @param listener
     */
    public Button_B23(B23Container values, OnClickListener listener)
    {
        super();
        container = values;
        mListener = listener;
    }

    /**
     * 
     * 
     *
     * @param listener
     */
    public void setListener(OnClickListener listener)
    {
        mListener = listener;
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
        return R.layout.button_b23;
    }

    /**
     * 
     * 
     *
     * @param group
     */
    @Override
    public void build(ViewGroup group)
    {
        // Added by Henry Tso
        mGroup = group;
        UIHelper.setText(group, R.id.id_b23_title, "Hallo Welt");

        UIHelper.setText(group, R.id.id_b23_title, container.title);
        // Modified by Henry Tso. Check the icon id no equal to zero is
        // necessary
        mHealthEventIcon = (ImageView) group.findViewById(R.id.id_b23_icon_1_1);
        if (container.title_icon1 != 0) // Health Event Icon
        {
            mHealthEventIcon.setImageResource(container.title_icon1);
            // UIHelper.setImage(group, R.id.id_b23_icon_1_1,
            // container.title_icon1);
        }
        mMealTimeIcon = (ImageView) group.findViewById(R.id.id_b23_icon_1_2);
        if (container.title_icon2 != 0) // Meal time icon
        {
            mMealTimeIcon.setImageResource(container.title_icon2);
            // UIHelper.setImage(group, R.id.id_b23_icon_1_2,
            // container.title_icon2);
        }
        mNoteIcon = (ImageView) group.findViewById(R.id.id_b23_icon_1_3);
        if (container.title_icon3 != 0)
        {
            mNoteIcon.setImageResource(container.title_icon3);
            // UIHelper.setImage(group, R.id.id_b23_icon_1_3,
            // container.title_icon3);
        }
        if (container.title_icon4 != 0)
        {
            UIHelper.setImage(group, R.id.id_b23_icon_1_4,
                    container.title_icon4);
        }
        if (container.unit1_icon != 0)
        {
            UIHelper.setImage(group, R.id.id_b23_icon_2, container.unit1_icon);
            UIHelper.setText(group, R.id.id_b23_value1, container.unit1_value);
        }
        if ((container.unit2_icon1 != 0) || (container.unit2_icon2 != 0))
        {
            if (container.unit2_icon1 != 0)
            {
                UIHelper.setImage(group, R.id.id_b23_icon_3_1,
                        container.unit2_icon1);
            }
            if (container.unit2_icon2 != 0)
            {
                UIHelper.setImage(group, R.id.id_b23_icon_3_2,
                        container.unit2_icon2);
            }
            UIHelper.setText(group, R.id.id_b23_value2, container.unit2_value);
        }
        mCarbsIcon = (ImageView) group.findViewById(R.id.id_b23_icon_4);
        mCarbsValue = (TextView) group.findViewById(R.id.id_b23_value3);
        if (container.unit3_icon != 0)
        {
            mCarbsIcon.setImageResource(container.unit3_icon);
            // UIHelper.setImage(group, R.id.id_b23_icon_4,
            // container.unit3_icon);

            mCarbsValue.setText(container.unit3_value);
            // UIHelper.setText(group, R.id.id_b23_value3,
            // container.unit3_value);
        }
        LinearLayout view = (LinearLayout) group
                .findViewById(R.id.id_button_b23);
        if (mListener != null)
        {

            view.setOnClickListener(mListener);
        }
        else
        {
            view.setBackgroundResource(R.color.background);
        }
    }

    /**
     * 
     * 
     *
     * @param group
     * @return
     */
    @Override
    public ViewGroup add(ViewGroup group)
    {
        // TODO Auto-generated method stub
        return null;
    }

    // Added by Henry Tso {
    /**
     * Call this API for getting the ImageView object by given the resource id
     * 
     * @param nResourceId [in] Resource id of the ImageView object
     *            Range: Valid Resource Id (> 0)
     *            Unit: int
     *            Scaling: 1
     * @return ImageView [out] ImageView object of the icon
     *         Range: Valid ImageView Object
     *         Unit: ImageView
     *         Scaling: 1
     * 
     * @see mGroup: ViewGroup object of the current button
     */
    public ImageView getIconViewById(int nResourceId)
    {
        ImageView iv = null;

        if (mGroup != null)
        {
            iv = (ImageView) mGroup.findViewById(nResourceId);
        }
        return iv;
    }

    /**
     * Call this API for setting the icon of Meal Time.
     * 
     */
    public void setMealTimeIcon()
    {
        mMealTimeIcon.setImageResource(R.drawable.mealtime);
    }

    /**
     * Call this API for setting the icon of Health Event
     */
    public void setHealthEventIcon()
    {
        mHealthEventIcon.setImageResource(R.drawable.health_event);
    }

    /**
     * Call this API for setting the icon of Carbs
     */
    public void setCarbsIcon()
    {
        mCarbsIcon.setImageResource(R.drawable.carbs);
    }

    /**
     * Call this API for setting the String of Carbs value
     * 
     * @param sValue [in] Carbs value with String object format
     *            Range: The String object must be contented the Value string
     *            Unit: String object
     *            Scaling: 1
     * 
     * @return void [out] None
     */
    public void setCarbsValue(String sValue)
    {
        mCarbsValue.setText(sValue);
    }

    /**
     * Call this API for setting the icon of Note
     * 
     * @return void [out] None
     */
    public void setNoteIcon()
    {
        mNoteIcon.setImageResource(R.drawable.note);
    }

    /**
     * Violate coding rule: R16
     */
    public static class B23Container
    {
        
        public String title;
        
        public int title_icon1;
        
        public int title_icon2;
        
        public int title_icon3;
        
        public int title_icon4;

        public int unit1_icon;
        
        public String unit1_value;

        public int unit2_icon1;
        
        public int unit2_icon2;
        
        public String unit2_value;

        public int unit3_icon;
        
        public String unit3_value;

    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Fix SCR0315 screen's U/I no update problem.
// Support note iocn display in SCR0315 screen
// Support note iocn display in SCR0315 screen
