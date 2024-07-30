Tar(:,1)=[16.5,7,6.5,14];
Tar(:,2)=[6,10,4,21];
cities = ...
{'Book','Stool','Football','Pillow'};
GroupOfId=[1,1,2,2,1,1,3,3,3,3,3,3,1,1,1,2,2,2,2,1,1,1,1,2,2,1,1,4,2,4,3,1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,3,4,4,3,4,4,3,3,4,3,3,3,3,2,2,4,2,2,2,2,2,1,2,2,3,4,2];
Tarnumber=1;

dim_x = 265;
dim_y = 185;
polys = cell(4,1);
if Tarnumber==4||Tarnumber==3
    polys{1} = [260 180, 0 180, 0 0, 260 0 , 260 180] + 2;
    %polys{2} = [170 150,170 180,120 180,120 105] + 2;%
    %polys{3} = [120 75,120 0,170 0,170 30]+2;
    %polys{4} = [260 90,170 90,170 60,170 120] + 2;%
elseif Tarnumber==1||Tarnumber==2
    polys{1} = [260 180, 0 180, 0 0, 260 0 , 260 180]+ 2;
    %polys{2} = [140 150,120 180,70 180,120 105] + 2;%
    %polys{3} = [140 75,190 0,240 0,220 30]+2;
    %polys{4} = [260 90,180 90,200 60,160 120] + 2;%
end

env = GenerateEnv(polys, dim_x, dim_y);%设置基础环境
name = strcat('Overall_');
title(name,'FontWeight','normal')
xx=[165,70,65,140]+2;%
yy=[60,100,40,210]+2;
hold on;%画普鲁克
n=0;
for i = 1:76  
    
    if GroupOfId(i)~=Tarnumber||Zans(i,1)==0
        continue;
    end
    plot([Zans(i,1),Zans(i,7)],[Zans(i,2),Zans(i,8)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,7),Zans(i,3)],[Zans(i,8),Zans(i,4)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,3),Zans(i,5)],[Zans(i,4),Zans(i,6)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,5),Zans(i,1)],[Zans(i,6),Zans(i,2)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    %Zans(i,:)=[Z(1,1),Z(1,2),Z(2,1),Z(2,2),Z(3,1),Z(3,2),Z(4,1),Z(4,2),];
    
end

Tarnumber=2;
for i = 1:76  
    if GroupOfId(i)~=Tarnumber||Zans(i,1)==0
        continue;
    end
    plot([Zans(i,1),Zans(i,7)],[Zans(i,2),Zans(i,8)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,7),Zans(i,3)],[Zans(i,8),Zans(i,4)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,3),Zans(i,5)],[Zans(i,4),Zans(i,6)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
    plot([Zans(i,5),Zans(i,1)],[Zans(i,6),Zans(i,2)],'.','linewidth',2,'markeredgecolor','b','markersize',10)
end

Tarnumber=3;
for i = 1:76  
    if GroupOfId(i)~=Tarnumber||Zans(i,1)==0
        continue;
    end
    plot([Zans(i,1),Zans(i,7)],[Zans(i,2),Zans(i,8)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,7),Zans(i,3)],[Zans(i,8),Zans(i,4)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,3),Zans(i,5)],[Zans(i,4),Zans(i,6)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,5),Zans(i,1)],[Zans(i,6),Zans(i,2)],'.','linewidth',2,'Color','red','markersize',10)
end

Tarnumber=4;
for i = 1:76  
    if GroupOfId(i)~=Tarnumber||Zans(i,1)==0
        continue;
    end
    plot([Zans(i,1),Zans(i,7)],[Zans(i,2),Zans(i,8)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,7),Zans(i,3)],[Zans(i,8),Zans(i,4)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,3),Zans(i,5)],[Zans(i,4),Zans(i,6)],'.','linewidth',2,'Color','red','markersize',10)
    plot([Zans(i,5),Zans(i,1)],[Zans(i,6),Zans(i,2)],'.','linewidth',2,'Color','red','markersize',10)
end

plot(xx(1),yy(1),'X','linewidth',2,'markeredgecolor','k','markersize',15)
plot(xx(2),yy(2),'X','linewidth',2,'markeredgecolor','k','markersize',15)
plot(xx(3),yy(3),'X','linewidth',2,'markeredgecolor','k','markersize',15)
plot(xx(4),yy(4),'X','linewidth',2,'markeredgecolor','k','markersize',15)

verti = zeros(1,8);
Unverti = zeros(1,8);
for i = 1:76  
    if Zans(i,1)==0
        continue;
    end
    if GroupOfId(i) == 3||GroupOfId(i) == 4
        verti(1)=verti(1)+Zans(i,1);verti(2)=verti(2)+Zans(i,2);
        verti(3)=verti(3)+Zans(i,3);verti(4)=verti(4)+Zans(i,4);
        verti(5)=verti(5)+Zans(i,5);verti(6)=verti(6)+Zans(i,6);
        verti(7)=verti(7)+Zans(i,7);verti(8)=verti(8)+Zans(i,8);
    elseif GroupOfId(i) == 1||GroupOfId(i) == 2
        Unverti(1)=Unverti(1)+Zans(i,1);Unverti(2)=Unverti(2)+Zans(i,2);
        Unverti(3)=Unverti(3)+Zans(i,3);Unverti(4)=Unverti(4)+Zans(i,4);
        Unverti(5)=Unverti(5)+Zans(i,5);Unverti(6)=Unverti(6)+Zans(i,6);
        Unverti(7)=Unverti(7)+Zans(i,7);Unverti(8)=Unverti(8)+Zans(i,8);
    end
end
verti=verti/26;
Unverti=Unverti/26;

line([verti(1),verti(7)],[verti(2),verti(8)],'linewidth',2,'Color','red','markersize',10)
line([verti(7),verti(3)],[verti(8),verti(4)],'linewidth',2,'Color','red','markersize',10)
line([verti(3),verti(5)],[verti(4),verti(6)],'linewidth',2,'Color','red','markersize',10)
line([verti(5),verti(1)],[verti(6),verti(2)],'linewidth',2,'Color','red','markersize',10)

line([Unverti(1),Unverti(7)],[Unverti(2),Unverti(8)],'linewidth',2,'Color','blue','markersize',10)
line([Unverti(7),Unverti(3)],[Unverti(8),Unverti(4)],'linewidth',2,'Color','blue','markersize',10)
line([Unverti(3),Unverti(5)],[Unverti(4),Unverti(6)],'linewidth',2,'Color','blue','markersize',10)
line([Unverti(5),Unverti(1)],[Unverti(6),Unverti(2)],'linewidth',2,'Color','blue','markersize',10)
%hold off;
fig(Tarnumber) = figure(1);
%picturename = strcat('D:\PPlot\Find P',num2str(Tarnumber),'.jpg');
%saveas(fig(Tarnumber),picturename,'jpg')   
%close all;

