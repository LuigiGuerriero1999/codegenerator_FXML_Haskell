package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

import java.util.ArrayList;

public class Layout extends GTKWidget{


    private double width;
    private double height;

    public Layout(double layoutX, double layoutY, double width, double height) {
        setLayoutX(layoutX);
        setLayoutY(layoutY);
        this.width = width;
        this.height = height;
    }
    public Layout(String id, Integer id_hash, String name, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container");
        setLayoutX(layoutX);
        setLayoutY(layoutY);
        this.width = width;
        this.height = height;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }



    public String gtkHsCode(){
        String layoutConstructor = super.getName() + " <- Gtk.layoutNew (Nothing::Maybe Adjustment) (Nothing::Maybe Adjustment)\n  ";
        String GtkHsCode = layoutConstructor + "\n  ";
        return GtkHsCode;

    }


}
