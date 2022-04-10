package com.example.testboy;

import com.example.testboy.structures.Button;
import com.example.testboy.structures.GTKWidget;

import java.util.ArrayList;

public class LayoutRelation extends Relation{
    public LayoutRelation() {
    }

    public LayoutRelation(Integer parentID, Integer childID) {
        super(parentID, childID);
    }

    @Override
    public String generateGtkHsCode(ArrayList<GTKWidget> allwidgets){
        String childWidgetName = "", layoutName = "";
        Integer x = 0, y = 0;
        for(GTKWidget element : allwidgets) {
            Integer childID = super.getChildID();
            Integer parentID = super.getParentID();
            Integer elementIDHash = element.getId_hash();
            if(childID.equals(elementIDHash)) {
                childWidgetName = element.getName();
                if(element instanceof Button){
                    x = (int)((Button) element).getLayoutX();
                    y = (int)((Button) element).getLayoutY();
                }
            }
            if(parentID.equals(elementIDHash)){
                layoutName = element.getName();
            }
        }
        String gtkHscode = "Gtk.layoutPut "+layoutName+" "+childWidgetName+" "+x+" "+y+"\n  ";
        return gtkHscode;
    }
}
