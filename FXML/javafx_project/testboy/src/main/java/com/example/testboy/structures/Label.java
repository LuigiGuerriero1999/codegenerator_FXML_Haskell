package com.example.testboy.structures;

public class Label extends GTKWidget{
    private String text;

    private double x;
    private double y;

    private double width;
    private double height;

    public Label(String text, double x, double y, double width, double height) {
        this.text = text;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    public Label(String id, Integer id_hash, String name, String text, double x, double y, double width, double height) {
        super(id, id_hash, name);
        this.text = text;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
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

        String labelGtkHsCode = super.getName() + " <- Gtk.labelNew (Just " + "\"" + text + "\"" + ") \n";

        return labelGtkHsCode;
    }
}
