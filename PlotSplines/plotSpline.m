function plotSpline(input, accuracy)
    if nargin==1
        accuracy=0.000001;
    end
    figure(1); clf; hold on;
    delimitedSpline =  regexp(input,'(','split');
    n = length(delimitedSpline);
    numSplines = n-1
    breakpoint=0;
    for i=2:n
       j = char(delimitedSpline(i));
       polySegment = regexp(j,',[','split');
       polySegment(1);
       duration=str2double(char(polySegment(1)));
       poly=char(polySegment(2));
       k=size(poly,2);
       if k<=3
           c='0';
       else
           poly=poly(1:k-3);
           c=regexp(poly,',','split');
       end
       coefficients=zeros(1,length(c));
       for p=1:length(c)
          coefficients(length(c) - p + 1) = str2double(char(c(p)));
       end
       x=breakpoint:accuracy:(duration+breakpoint);
       y=polyval(coefficients, 0:accuracy:duration);
       plot(x,y);
       breakpoint=breakpoint+duration;
    end
    splineEnd = breakpoint
end