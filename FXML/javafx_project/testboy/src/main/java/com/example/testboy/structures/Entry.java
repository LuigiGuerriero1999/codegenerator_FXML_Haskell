package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

public class Entry extends GTKWidget{
    private String text;
    private String entryName;

    private double layoutX;
    private double layoutY;

    private double width;
    private double height;

    public Entry(String text, double layoutX, double layoutY, double width, double height) {
        this.text = text;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
    }

    public Entry(String id, Integer id_hash, String name, String text, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container");
        this.text = text;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
        this.entryName = makeName(id,id_hash,name);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
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

    public String gtkHsCode() {
        String entryConstructor = entryName + " <- Gtk.entryNew\n  ";
        String entryContainerBoxName = super.getName();
        String createEntryContainer = entryContainerBoxName + " <- Gtk.boxNew OrientationHorizontal 1\n  ";
        String setEntryContainerProperties = "Gtk.set "+ entryContainerBoxName +" [Gtk.widgetWidthRequest := "+(int)width+", Gtk.widgetHeightRequest := "+(int)height+"]\n  ";
        String addEntryToContainer = "Gtk.boxPackStart "+ entryContainerBoxName +" "+ entryName +" True True 0\n   ";
        String entryGtkHsCode = entryConstructor + createEntryContainer + setEntryContainerProperties + addEntryToContainer;

        return entryGtkHsCode;
    }
}
