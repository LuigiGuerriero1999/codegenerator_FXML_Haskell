package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

public class Layout extends GTKWidget{
    private double layoutX;
    private double layoutY;

    private double width;
    private double height;



    public Layout(double layoutX, double layoutY, double width, double height) {
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
    }

    public Layout(String id, Integer id_hash, String name, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, HelloApplication.makeName(id,id_hash,name)+"Container");
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
    }

    public double getLayoutX() {
        return layoutX;
    }

    public void setLayoutX(double layoutX) {
        this.layoutX = layoutX;
    }

    public double getLayoutY() {
        return layoutY;
    }

    public void setLayoutY(double layoutY) {
        this.layoutY = layoutY;
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
        String GtkHsCode = layoutConstructor;

        return GtkHsCode;

    }
}
