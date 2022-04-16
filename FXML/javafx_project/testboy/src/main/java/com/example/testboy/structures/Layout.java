package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

import java.util.ArrayList;

public class Layout extends GTKWidget{
    public Layout(double layoutX, double layoutY) {
        setLayoutX(layoutX);
        setLayoutY(layoutY);
    }
    public Layout(String id, Integer id_hash, String name, double layoutX, double layoutY) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
    }

    public String gtkHsCode(){
        String layoutConstructor = super.getName() + " <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)\n  ";
        String GtkHsCode = layoutConstructor + "\n  ";
        return GtkHsCode;
    }
}
