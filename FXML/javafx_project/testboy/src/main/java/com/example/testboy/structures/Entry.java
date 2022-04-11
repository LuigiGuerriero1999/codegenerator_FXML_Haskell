package com.example.testboy.structures;

public class Entry extends GTKWidget{
    private String text;

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
        super(id, id_hash, name);
        this.text = text;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
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
        String entryGtkHsCode = super.getName() + " <- Gtk.entryNew ";
        return entryGtkHsCode;
    }
}
