/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.ContinuaCommandController.data.IContinuaCommandParser
 * Brief: 
 *
 * Create Date: 2015/3/19
 * $Revision: 23982 $
 * $Author: kevenwu $
 * $Id: ProductIdentifierHandler.java 23982 2015-11-12 09:22:48Z kevenwu $
 */

package com.accu_chek.solo_m.rcapp.application.continua.typehandler;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.continua.commandcontorller.ContinuaCommandSet;
import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenProductionModel;

public class ProductIdentifierHandler implements IContinuaCommandHandler 
{
	/**
	 * Get the product identifier from production model and transfers it to Continua Agent.
     * 
     * @param commandSet : The instance of Continua command set.
     *        Range: Valid object of ContinuaCommandSet.
     *        Unit: ContinuaCommandSet.
     *        Scaling: 1.
     *        
     * return void [out]: None
	 */
	@Override
	public void handleCommand(ContinuaCommandSet commandSet) 
	{
		final int RADIX_OF_HEX = 16;
		final int START_INDEX = 2;
		
		SafetyString key = new SafetyString(ProductionConstants.KEY_PRODUCT_IDENTIFIER,
		        CRCTool.generateCRC16(ProductionConstants.KEY_PRODUCT_IDENTIFIER.getBytes()));
		SafetyString productId = NugenProductionModel.getString(key);
		StringBuilder builder = new StringBuilder();
		
		productId = new SafetyString("01-95-00-00-43", CRCTool.generateCRC16("01-95-00-00-43".getBytes()));
		
		String[] id = productId.getString().split("-");
		
		for (int i=START_INDEX; i<id.length; i++)
		{
			builder.append(id[i]);
		}
		
		commandSet.getController().setSegmentDataToAgent(ContinuaCommand.PRODUCT_IDENTIFIER, 
				ParseUtils.appendCRC(ParseUtils.parseInt32(Integer.parseInt(builder.toString(), RADIX_OF_HEX))));
	}
}
