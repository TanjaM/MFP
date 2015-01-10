%function usage example:
%  s1 = '[(1,[0,1])]'
%  s2 = '[(1,[1,-1])]'
%  s3 = ...
%  s={s1,s2, s3, ...}
%  plotSpline(s)    ... ploting several splines
%  plotSpline({s1}) ... plot one spline

function plotSpline(splineList)
    if nargin==1
        accuracy=0.001;
    end
    figure(1); clf; hold on;
    n = length(splineList);
    colors = ['b','r','g','m','c','w','k', 'y'];
    for i=1:n
       plotingSpline = i
       plotPolySegment(char(splineList(i)), accuracy, colors(mod(i,length(colors)))) ;
    end
end

function plotPolySegment(input, accuracy, color)
    delimitedSpline =  regexp(input,'(','split');
    n = length(delimitedSpline);
    numPolySegments = n-1
    breakpoint=0;
    for i=2:n
       j = char(delimitedSpline(i));
       polySegment = regexp(j,',[','split');
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
           coefficients(length(c)+1-p)= str2double(char(c(p)));
       end
       x=breakpoint:accuracy:(duration+breakpoint);
       y=polyval(coefficients, 0:accuracy:duration);
       plot(x,y,'Color',color);
       breakpoint=breakpoint+duration;
    end
    splineEnd = breakpoint
end
