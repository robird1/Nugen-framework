/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ButtonBasic
 * Brief: This ButtonBasic class is the button base class
 *
 * Create Date: 12/09/2014
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: ButtonBasic.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.uitools;

import android.app.Activity;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.accu_chek.solo_m.rcapp.application.gui.globaltools.GlobalTools;

public class ButtonBasic implements UIBuilder
{
    
    protected static final int DEFAULT_CLICK_ID = R.id.id_button_white_field;
    
    // This index is designed for representing the button index
    // Added by Henry Tso
    protected int mItemIndex = 0;
    
    int mText = 0;
    
    String mStringText = null;
    
    private ViewGroup mButtonView = null;
    
    private OnClickListener mListener = null;
    
    private int mTag = 0;
    
    // Default true
    private boolean mClickable = true;
    
    // One more listener for listening the "Check box" event only
    // Added by Henry Tso
    private OnClickListener mCheckBoxListener = null;

    /**
     * 
     * @param stringText
     */
    public ButtonBasic(String stringText)
    {
        super();
        mStringText = stringText;
    }

    /**
     * 
     * @param mText
     */
    public ButtonBasic(int mText)
    {
        super();
        this.mText = mText;
    }

    /**
     * 
     * @param stringText
     * @param listener
     */
    public ButtonBasic(String stringText, OnClickListener listener)
    {
        this(stringText);
        mListener = listener;
    }

    /**
     * 
     * @param stringText
     * @param listener
     * @param tag
     */
    public ButtonBasic(String stringText, OnClickListener listener, int tag)
    {
        this(stringText);
        mListener = listener;
        mTag = tag;
    }

    /**
     * 
     * @param mText
     * @param listener
     */
    public ButtonBasic(int mText, OnClickListener listener)
    {
        this(mText);
        mListener = listener;
    }

    /**
     * 
     */
    public ButtonBasic()
    {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * @return Text id
     */
    public int getTextId()
    {
        return mText;
    }

    /**
     * @return Text string if set
     */
    public String getText()
    {
        return mStringText;
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
        setButtonText(group, getTextViewId());
        addOnClickListener(group);
        updateClicable(group);
        
        if (GlobalTools.ReplaceLenghtWithDots)
        {
            enableEllipsize(group);
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

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getLayoutId()
    {
        return R.layout.button_spacing6px;
    }

    /**
     * modified by Henry
     *
     * Call this API to create the view of the button item.
     *
     * @param activity [in] Current Activity object
     * @param parent [in] Parent class object
     * @param buttonIndex [in] I
     * 
     * @return ViewGroup [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public ViewGroup createView(Activity activity, ViewGroup parent,
            int buttonIndex)
    {
        // cache result
        mButtonView = createView(activity, getLayoutId(), parent);
        mItemIndex = buttonIndex;
        setView(mButtonView);
        return mButtonView;
    }

    /**
     * 
     * Function Description
     *
     * @param activity
     * @param parent
     * @param buttonIndex
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    public ViewGroup createDatailBgView(Activity activity, ViewGroup parent,
            int buttonIndex)
    {
        // cache result
        mButtonView = createView(activity, getLayoutId(), parent);
        mItemIndex = buttonIndex;
        setDetailBgView(mButtonView);
        return mButtonView;
    }
    
    /**
     * 
     * Function Description
     *
     * @param mInflater
     * @param buttonContainer
     * @return
     * @return View [out] Delete pre line return if exist. Parameter Description
     */
    public View createView(LayoutInflater mInflater, ViewGroup buttonContainer)
    {
        // TODO Auto-generated method stub
        mButtonView = (ViewGroup) mInflater.inflate(getLayoutId(),
                buttonContainer, false);
        setView(mButtonView);
        return mButtonView;
    }

    /**
     * 
     * Function Description
     *
     * @param listener
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setListener(OnClickListener listener)
    {
        this.mListener = listener;
    }

    /**
     * Add one more Listener for listening the Check box event only. When the
     * user clicking the button. The U/I will want to know which button actually
     * was clicked. The Id of this button will be returned to upper layer for
     * identify which list item was clicked.
     *
     * @param buttonListener [in] The button's listener
     *            Range: Valid OnClickListener of the button itself
     *            Unit: OnClickListener object
     *            Scaling: 1
     * 
     * @param checkBoxListener [in] The checkbox's listener
     *            Range: Valid OnClickListener of the checkbox itself
     *            Unit: OnClickListener object
     *            Scaling: 1
     * 
     * @return void [out] None
     */
    public void setCheckBoxIconListener(OnClickListener checkBoxListener)
    {
        this.mCheckBoxListener = checkBoxListener;
    }

    /**
     * 
     * Function Description
     *
     * @param tag
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTag(int tag)
    {
        this.mTag = tag;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter Description
     */
    public boolean isClickable()
    {
        return mClickable;
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return
     * @return ButtonBasic [out] Delete pre line return if exist. Parameter Description
     */
    public ButtonBasic setIsClickable(boolean value)
    {
        mClickable = value;
        return this;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return OnClickListener [out] Delete pre line return if exist. Parameter Description
     */
    public OnClickListener getListener()
    {
        return mListener;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    public ViewGroup getView()
    {
        return mButtonView;
    }

    /**
     * 
     * Function Description
     *
     * @param group
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void buildDetailBgResult(ViewGroup group)
    {
        
    }

    /**
     * 
     * Function Description
     *
     * @param valueId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateText(int valueId)
    {
        mText = valueId;
        mStringText = null;
        setButtonText(getView(), getTextViewId());
    }

    /**
     * 
     * Function Description
     *
     * @param value
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateText(String value)
    {
        mStringText = value;
        setButtonText(getView(), getTextViewId());
    }

    /**
     * 
     * Function Description
     *
     * @param selected
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setSelected(boolean selected)
    {

    }
    
    /**
     * Call this function for setting the item index.
     * This item index is reserved for UIAutomator usage.
     * Added by Henry Tso
     *
     * @param nItemIndex [in] Item index of the current button object
     *            Range: 0 ~ n (n is the maximum of the ListView item)
     *            Unit: Integer
     *            Scaling: 1
     * @return None
     */
    public void setBaseItemIndex(int nItemIndex)
    {
        mItemIndex = nItemIndex;
    }

    /**
     * Call this API for getting the Item index.
     *
     * @return int [out] Item index.
     */
    public int getItemIndex()
    {
        return mItemIndex;
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    protected int getTextViewId()
    {
        return R.id.id_button_text;
    }
    
    /**
     * 
     * Function Description
     *
     * @param activity
     * @param layoutId
     * @param parent
     * @return
     * @return ViewGroup [out] Delete pre line return if exist. Parameter Description
     */
    protected ViewGroup createView(Activity activity, int layoutId,
            ViewGroup parent)
    {
        LayoutInflater inflater = activity.getLayoutInflater();
        return (ViewGroup) inflater.inflate(layoutId, parent, false);
    }
    
    /**
     * 
     * Function Description
     *
     * @param parent
     * @param viewId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setButtonText(ViewGroup parent, int viewId)
    {
        // Add by Henry Tso. Used by UIAutomator
        String sIndex = String.format("Item_%d", mItemIndex);

        if (mStringText == null)
        {
            UIHelper.setText(parent, viewId, mText, sIndex);
        }
        else
        {
            UIHelper.setText(parent, viewId, mStringText, sIndex);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param parent
     * @param viewId
     * @param StringId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setButtonText(ViewGroup parent, int viewId, int StringId)
    {
        // Add by Henry Tso. Used by UIAutomator
        String sIndex = String.format("Item_%d", mItemIndex);

        UIHelper.setText(parent, viewId, StringId, sIndex);
    }
    
    /**
     * 
     * Function Description
     *
     * @param rowView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void addOnClickListener(View rowView)
    {
        addOnClickListener(rowView, DEFAULT_CLICK_ID);
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void updateClicable()
    {
        updateClickable(mButtonView, DEFAULT_CLICK_ID);

    }

    /**
     * 
     * Function Description
     *
     * @param mButtonView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void updateClicable(ViewGroup mButtonView)
    {
        updateClickable(mButtonView, DEFAULT_CLICK_ID);

    }

    /**
     * 
     * Function Description
     *
     * @param rowView
     * @param clickableId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void updateClickable(View rowView, int clickableId)
    {
        if (!isClickable())
        {
            View clickfield = rowView.findViewById(clickableId);
            clickfield.setClickable(false);
            clickfield.setBackgroundResource(R.color.background);
        }
    }

    /**
     * 
     * Function Description
     *
     * @param rowView
     * @param clickableId
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void addOnClickListener(View rowView, int clickableId)
    {
        if (mListener != null)
        {
            View view = rowView.findViewById(clickableId);
            if (view != null)
            {
                view.setOnClickListener(mListener);
                view.setTag(mTag);
            }
        }
    }

    /**
     * Addition method for setting up the listener of checkbox icon.
     * In current design. Only the button body has implemented the click
     * listener, when the user clicking the checkbox icon. The U/I will no way
     * to get the id of clicked item. This is the reason why need to add this
     * listener for checkbox U/I
     *
     * @param rowView
     * @param clickableId
     * @param checkboxId
     * @return void [out] None
     */
    protected void addCheckboxOnClickListener(View rowView, int clickableId,
            int checkboxId)
    {
        if (mListener != null)
        {
            View view = rowView.findViewById(clickableId);
            if (view != null)
            {
                view.setOnClickListener(mListener);
                view.setTag(mTag);
            }
        }
        // Added by Henry Tso to support click event listener on "Checkbox"
        if (mCheckBoxListener != null)
        {
            View view = rowView.findViewById(checkboxId);
            if (view != null)
            {
                view.setOnClickListener(mCheckBoxListener);
                view.setTag(mTag);
            }
        }
    }

    /**
     * 
     * Function Description
     *
     * @param buttonView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setView(ViewGroup buttonView)
    {
        mButtonView = buttonView;
        build(buttonView);
    }

    /**
     * 
     * Function Description
     *
     * @param buttonView
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    protected void setDetailBgView(ViewGroup buttonView)
    {
        mButtonView = buttonView;
        buildDetailBgResult(buttonView);
    }
    
    /**
     * 
     * Function Description
     *
     * @param group
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void enableEllipsize(ViewGroup group)
    {
        TextView tv = (TextView) group.findViewById(getTextViewId());
        tv.setEllipsize(TextUtils.TruncateAt.END);
    }

}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// Fix the checkbox issue in Health event selection screen.
